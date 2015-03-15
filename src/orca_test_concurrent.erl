-module (orca_test_concurrent).
-compile (export_all).

db_url() -> <<"mysql://orca_test:orca_test_password@localhost/orca_test_db?pool_size=10&min_restart_interval=500">>.
db_opts() -> [
			{callback_log, fun ?MODULE:log/2},
			{callback_log_tcp, fun ?MODULE:log_tcp/3}
		].
verbose_tcp() -> {callback_log_tcp, fun orca_default_callbacks:log_tcp_error_logger/3}.
verbose_log() -> {callback_log, fun orca_default_callbacks:log_error_logger/2}.


tasks_count() -> 1000.

thread( Orca, Idx ) ->
	ok = lists:foreach( fun( TaskID ) ->
			ok = io:format("~p ~p before~n", [ Idx, TaskID ]),
			{ok, _} = orca:sql( Orca, <<"SELECT 1 FROM DUAL">>, [] ),
			ok = io:format("~p ~p after~n", [ Idx, TaskID ])
		end, lists:seq( 1, tasks_count() ) ).

start() ->
	{ok, Orca} = orca:start_link( db_url(), db_opts() ),
	ok = timer:sleep(100),
	ok = orca:await_ready( Orca ),
	Threads = lists:map(
		fun ( ThreadID ) ->
			spawn( ?MODULE, thread, [ Orca, ThreadID ] )
		end,
		lists:seq( 1, 12 ) ),
	wait_all_monitor( Threads ),
	wait_all_receive( Threads ).


wait_all_monitor( [] ) -> ok;
wait_all_monitor( [ P | Ps ] ) ->
	_Ref = erlang:monitor( process, P ),
	wait_all_monitor( Ps ).

wait_all_receive( [] ) -> ok;
wait_all_receive( [ P | Ps ] ) ->
	receive
		{ 'DOWN', _Ref, process, P, Reason } ->
			ok = io:format("worker ~p:~p~n", [ P, Reason ]),
			wait_all_receive( Ps )
	end.

log_tcp( Conn, Direction, Binary ) ->
	io:format("~p\t~p\t~999p~n", [ Conn, Direction, Binary ]).

log( Lvl, Report ) ->
	io:format("~p\t\t\t~999p~n", [ Lvl, Report ]).

decode_packet_payload_sequence( Payloads ) ->
	lists:foldl(
		fun ( Bin, Ctx0 ) ->
			case orca_decoder_generic_response:decode_continue( Bin, Ctx0 ) of
				{incomplete, Ctx1} -> Ctx1;
				{ok, Result} ->
					ok = io:format("> ~p~n", [ Result ]),
					undefined
			end
		end,
		undefined, Payloads ).


