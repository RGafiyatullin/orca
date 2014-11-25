-module (orca_conn).
-compile ({parse_transform, gin}).
-export ([
		start_link/1, start_link/2,
		shutdown/2,
		init_db/2,
		ping/1,
		sql/2, sql/3,
		process_info/1,
		process_kill/2,
		quit/1,
		raw_packet_and_recv/2,
		raw_packet/3,
		recv_response/1
	]).
-include ("orca.hrl").
-include ("types.hrl").

-type generic_response() :: #orca_ok{} | #orca_rows{} | #orca_error{} | #orca_request_local_file_content{}.
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

start_link( Url, Opts ) ->
	{ok, ConnProps} = orca_url:parse( Url ),
	ConnHost = proplists:get_value( host, ConnProps ),
	ConnPort = proplists:get_value( port, ConnProps ),
	ConnUser = proplists:get_value( user, ConnProps ),
	ConnPassword = proplists:get_value( password, ConnProps ),
	ConnDbName = proplists:get_value( db_name, ConnProps ),

	ClientCapFlags = orca_caps:cap_flags_from_opts( Opts ),

	{ok, Conn} = orca_conn_srv:start_link(ConnHost, ConnPort, [{active, false} | Opts]),
	{ok, PacketHandshakeReq} = orca_conn_srv:recv_packet( Conn ),
	{ok, {handshake_request, PropsHandshakeReq}} = orca_decoder_handshake:decode( PacketHandshakeReq ),
	{ok, PacketHandshakeResp} = orca_encoder_handshake_response:auth(
		ConnUser, ConnPassword, ConnDbName, ClientCapFlags, [], PropsHandshakeReq ),
	ok = orca_conn_srv:send_packet( Conn, 1, PacketHandshakeResp ),
	{ok, PacketAuthResult} = orca_conn_srv:recv_packet( Conn ),
	{ok, {TypeAuthResult, PropsAuthResult}} = orca_decoder_generic_response:decode( PacketAuthResult ),
	case TypeAuthResult of
		ok_packet ->
			{ok, PacketInitDbReq} = orca_encoder_com:com_init_db( ConnDbName ),
			ok = orca_conn_srv:send_packet( Conn, 0, PacketInitDbReq ),
			{ok, PacketInitDbResp} = orca_conn_srv:recv_packet( Conn ),
			{ok, {TypeInitDb, PropsInitDb}} = orca_decoder_generic_response:decode( PacketInitDbResp ),
			case TypeInitDb of
				ok_packet -> {ok, Conn};
				err_packet -> {error, {init_db, PropsInitDb}}
			end;
		err_packet -> {error, {auth, PropsAuthResult}}
	end.

shutdown( OrcaConn, Reason ) when is_pid( OrcaConn ) ->
	ok = orca_conn_srv:shutdown( OrcaConn, Reason ).

init_db( C, Db ) ->
	{ok, ComInitDb} = orca_encoder_com:com_init_db( Db ),
	raw_packet_and_recv( C, ComInitDb ).

ping( C ) ->
	{ok, ComPing} = orca_encoder_com:com_ping(),
	raw_packet_and_recv( C, ComPing ).

sql( C, Q ) -> sql( C, Q, [] ).
sql( C, Q, A ) ->
	{ok, ComQuery} = orca_encoder_com:com_query( Q, A ),
	raw_packet_and_recv( C, ComQuery ).

quit( C ) ->
	{ok, ComQuit} = orca_encoder_com:com_quit(),
	raw_packet_and_recv( C, ComQuit ).

process_info( C ) ->
	{ok, ComProcessInfo} = orca_encoder_com:com_process_info(),
	raw_packet_and_recv( C, ComProcessInfo ).

process_kill( C, ThreadID ) ->
	{ok, ComKill} = orca_encoder_com:com_process_kill( ThreadID ),
	raw_packet_and_recv( C, ComKill ).

raw_packet_and_recv( C, Packet ) ->
	ok = orca_conn_srv:send_packet( C, 0, Packet ),
	recv_response( C ).

raw_packet( C, SeqID, Packet ) ->
	ok = orca_conn_srv:send_packet( C, SeqID, Packet ).

recv_response( C ) ->
	recv_response( C, orca_conn_srv:recv_packet( C ), undefined ).

%%% Internal %%%

recv_response( _C, {error, RecvError}, _ ) -> {error, {recv, RecvError}};
recv_response( C, {ok, ResultBin}, RespCtx0 ) ->
	case orca_decoder_generic_response:decode_continue( ResultBin, RespCtx0 ) of
		{incomplete, RespCtx1} -> recv_response( C, orca_conn_srv:recv_packet( C ), RespCtx1 );
		{ok, {PacketType, PacketProps}} -> result( PacketType, PacketProps )
	end.

result( ok_packet, Props ) ->
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
result( result_set_raw, PropsRaw ) ->
	{ok, Props} = orca_decoder_result_set:decode( PropsRaw ),
	result( result_set, Props );
result( result_set, Props ) ->
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
result( err_packet, Props ) ->
	OrcaError = lists:foldl(
		fun
			({err_code, V}, Acc) -> Acc #orca_error{ code = V };
			({err_msg, V}, Acc) -> Acc #orca_error{ msg = V };
			({sql_state_marker, V}, Acc) -> Acc #orca_error{ sql_state_marker = V };
			({sql_state, V}, Acc) -> Acc #orca_error{ sql_state = V };
			(_, Acc) -> Acc
		end,
		#orca_error{}, Props),
	{ok, OrcaError};

result( request_local_file_content, Props ) ->
	OrcaReqLocalFile = lists:foldl(
		fun
			({filename, Filename}, Acc) -> Acc #orca_request_local_file_content{ filename = Filename };
			(_, Acc) -> Acc
		end,
		#orca_request_local_file_content{}, Props),
	{ok, OrcaReqLocalFile}.
