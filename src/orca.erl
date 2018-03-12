-module (orca).
-export ([
		start_link/1, start_link/2,
		await_ready/3, await_ready/2, await_ready/1,
		shutdown/2
	]).
-export ([
		ping/1,
		sql/3, sql/2,
		process_info/1,
		process_kill/2
		% create_db/2, drop_db/2,
		% raw_packet/2
	]).

-include ("orca.hrl").
-include ("types.hrl").
-include ("orca_time.hrl").


-type generic_response() :: #orca_ok{} | #orca_rows{} | #orca_error{}.
-type query_text() :: iolist() | binary().

-spec start_link( db_url() ) -> {ok, pid()}.
-spec start_link( db_url(), [ conn_opt() ] ) -> {ok, pid()}.
-spec ping( pid() ) -> {ok, #orca_ok{}} | {error, term()}.
-spec sql( pid(), query_text() ) -> {ok, generic_response()} | {error, term()}.
-spec sql( pid(), query_text(), [ term() ] ) -> {ok, generic_response()} | {error, term()}.
-spec process_info( pid() ) -> {ok, #orca_rows{} | #orca_error{}} | {error, term()}.
-spec process_kill( pid(), non_neg_integer() ) -> {ok, #orca_ok{} | #orca_error{}} | {error, term()}.
-spec shutdown( pid(), Reason :: term() ) -> ok.

start_link( Url ) -> start_link( Url, [] ).
start_link( Url, Opts ) -> orca_conn_mgr:start_link( Url, Opts ).

await_ready( ConnMgr ) -> await_ready( ConnMgr, 5000, 100 ).
await_ready( ConnMgr, Timeout ) -> await_ready( ConnMgr, Timeout, 100 ).
await_ready( ConnMgr, Timeout, PingInterval )
	when is_integer(Timeout) andalso Timeout > 0
	andalso is_integer(PingInterval) andalso PingInterval > 0
->
	Now = ?now_ms,
	Deadline = Now + Timeout,
	await_ready_impl( ConnMgr, PingInterval, Deadline ).

shutdown( ConnMgr, Reason ) -> orca_conn_mgr:shutdown( ConnMgr, Reason ).

ping( ConnMgr ) ->
	{ok, ComPing} = orca_encoder_com:com_ping(),
	raw_packet( ConnMgr, ComPing ).

sql( ConnMgr, Query ) -> sql( ConnMgr, Query, [] ).
sql( ConnMgr, Query, Args ) ->
	{ok, ComQuery} = orca_encoder_com:com_query( Query, Args ),
	raw_packet( ConnMgr, ComQuery ).

process_info( ConnMgr ) ->
	{ok, ComProcessInfo} = orca_encoder_com:com_process_info(),
	raw_packet( ConnMgr, ComProcessInfo ).

% create_db( ConnMgr, DbName ) ->
% 	{ok, ComCreateDb} = orca_encoder_com:com_create_db( DbName ),
% 	raw_packet( ConnMgr, ComCreateDb ).
% drop_db( ConnMgr, DbName ) ->
% 	{ok, ComDropDb} = orca_encoder_com:com_drop_db( DbName ),
% 	raw_packet( ConnMgr, ComDropDb ).

process_kill( ConnMgr, ThreadID ) ->
	{ok, ComKill} = orca_encoder_com:com_process_kill( ThreadID ),
	raw_packet( ConnMgr, ComKill ).

raw_packet( ConnMgr, PacketBin ) ->
	result( orca_conn_mgr:execute( ConnMgr, PacketBin ) ).

result( Error = {error, _} ) -> Error;
result( {ok, {ok_packet, Props}} ) ->
	OrcaOk = lists:foldl(
		fun
			({affected_rows, V}, Acc) -> Acc #orca_ok{ affected_rows = V };
			({last_inserted_id, V}, Acc) -> Acc #orca_ok{ last_inserted_id = V };
			({status_flags, V}, Acc) -> Acc #orca_ok{ status_flags = V };
			({number_of_warnings, V}, Acc) -> Acc #orca_ok{ number_of_warnings = V };
			({msg, V}, Acc) -> Acc #orca_ok{ msg = V };
			(_, Acc) -> Acc
		end,
		#orca_ok{}, Props),
	{ok, OrcaOk};
result( {ok, {result_set, Props}} ) ->
	OrcaRows = lists:foldl(
		fun
			({rows, V}, Acc) -> Acc #orca_rows{ rows = V };
			({field_defs, V}, Acc) -> Acc #orca_rows{ field_defs = V };
			({status_flags, V}, Acc) -> Acc #orca_rows{ status_flags = V };
			({number_of_warnings, V}, Acc) -> Acc #orca_rows{ number_of_warnings = V };
			(_, Acc) -> Acc
		end,
		#orca_rows{}, Props),
	{ok, OrcaRows};
result( {ok, {err_packet, Props}} ) ->
	OrcaError = lists:foldl(
		fun
			({err_code, V}, Acc) -> Acc #orca_error{ code = V };
			({err_msg, V}, Acc) -> Acc #orca_error{ msg = V };
			({sql_state_marker, V}, Acc) -> Acc #orca_error{ sql_state_marker = V };
			({sql_state, V}, Acc) -> Acc #orca_error{ sql_state = V };
			(_, Acc) -> Acc
		end,
		#orca_error{}, Props),
	{ok, OrcaError}.


await_ready_impl( ConnMgr, PingInterval, Deadline ) ->
	case orca:ping( ConnMgr ) of
		{ok, #orca_ok{}} -> ok;
		{error, {lb, no_workers}} ->
			case ?now_ms < Deadline of
				true ->
					ok = timer:sleep( PingInterval ),
					await_ready_impl( ConnMgr, PingInterval, Deadline );
				false ->
					{error, timeout}
			end
	end.