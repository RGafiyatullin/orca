-module (orca_conn_mgr).
-compile ({parse_transform, gin}).
-behaviour (gen_server).

-export ([start_link/2]).
-export ([shutdown/2]).
-export ([execute/2, execute/3]).
-export ([
		init/1, enter_loop/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
	]).
-include("types.hrl").

-define(callback_log, {?MODULE, callback_log}).
-define(hib_timeout, 5000).
-define(is_timeout( Timeout ), ( Timeout == infinity orelse (is_integer(Timeout) andalso Timeout > 0) ) ).
-define(execute( PacketBin ), {execute, PacketBin}).

-define(
	init_args( User, Password, Host, Port, Database, PoolSize, MinRestartInterval, ConnOpts ),
	{init_args, User, Password, Host, Port, Database, PoolSize, MinRestartInterval, ConnOpts} ).
-define(
	orca_packet( ConnSrv, PacketBin ),
	{orca_packet, ConnSrv, PacketBin} ).
-define(
	worker_start(Idx),
	{worker_start, Idx}).
-define(
	shutdown( Reason ),
	{shutdown, Reason} ).

-spec start_link( db_url(), [ conn_opt() ] ) -> {ok, pid()}.
-spec shutdown( pid(), term() ) -> ok.
-spec execute( pid(), binary() ) ->
	{ok, {ok_packet | err_packet | result_set, [ {atom(), term()} ]}} | {error, term()}.
-spec execute( pid(), binary(), timeout() ) ->
	{ok, {ok_packet | err_packet | result_set, [ {atom(), term()} ]}} | {error, term()}.

start_link( Url, ConnOpts ) when is_binary( Url ) ->
	start_link( binary_to_list( Url ), ConnOpts );
start_link( Url = [ $m, $y, $s, $q, $l, $:, $/, $/ | _ ], ConnOpts ) ->
	{ok, ConnProps} = orca_url:parse( Url ),
	ConnHost = proplists:get_value( host, ConnProps ),
	ConnPort = proplists:get_value( port, ConnProps ),
	ConnUser = proplists:get_value( user, ConnProps ),
	ConnPassword = proplists:get_value( password, ConnProps ),
	ConnDbName = proplists:get_value( db_name, ConnProps ),

	PoolSize = proplists:get_value( pool_size, ConnProps ),
	MinRestartInterval = proplists:get_value( min_restart_interval, ConnProps ),

	InitArgs = ?init_args(
					ConnUser, ConnPassword,
					ConnHost, ConnPort,
					ConnDbName, PoolSize, MinRestartInterval,
					ConnOpts ),
	proc_lib:start_link( ?MODULE, enter_loop, [InitArgs] ).

shutdown( Srv, Reason ) ->
	gen_server:call( Srv, ?shutdown( Reason ) ).

execute( Srv, PacketBin ) ->
	execute_result( gen_server:call( Srv, ?execute( PacketBin ) ) ).

execute( Srv, PacketBin, Timeout )
	when is_pid( Srv )
	andalso is_binary( PacketBin )
	andalso ?is_timeout( Timeout )
->
	execute_result( gen_server:call( Srv, ?execute( PacketBin ), Timeout ) ).

execute_result( {error, Error} ) -> {error, Error};
execute_result( {ok, {TypeToBePassedAsIs, Props}} )
	when in(TypeToBePassedAsIs, [ok_packet, err_packet])
	-> {ok, {TypeToBePassedAsIs, Props}};
execute_result( {ok, {result_set_raw, RawProps}} ) ->
	{ok, Props} = orca_decoder_result_set:decode( RawProps ),
	{ok, {result_set, Props}}.

-record(conn_props, {
		user :: binary(),
		password :: binary(),
		host :: inet_host(),
		port :: inet_port(),
		database :: binary()
	}).
-record(pool_worker, {
		idx :: non_neg_integer(),
		pid :: undefined | pid(),
		mon :: undefined | reference(),

		last_start = 0 :: integer()
	}).
-record(conn_pool, {
		sup :: pid(),
		workers = [] :: [ #pool_worker{} ],
		min_restart_interval :: non_neg_integer()
	}).
-record(s, {
		client_cap_flags :: non_neg_integer(),
		lb :: orca_conn_mgr_lb:ctx(),
		pool_size :: pos_integer(),
		conn_props :: #conn_props{},
		conn_pool :: #conn_pool{}
	}).

init( _ ) -> {stop, {error, enter_loop_used}}.
enter_loop(?init_args( User, Password, Host, Port, Database, PoolSize, MinRestartInterval, ConnOpts )) ->
	LogF = proplists:get_value( callback_log, ConnOpts, fun orca_default_callbacks:log_error_logger/2 ),
	undefined = erlang:put( ?callback_log, LogF ),

	ClientCapFlags = orca_caps:cap_flags_from_opts( ConnOpts ),
	ConnProps = #conn_props{
			user = User,
			password = Password,
			host = Host,
			port = Port,
			database = Database
		},
	{ok, ConnPool} = init_conn_pool( PoolSize, MinRestartInterval, Host, Port, ConnOpts ),
	{ok, LB} = orca_conn_mgr_lb:new(),
	S0 = #s{
			client_cap_flags = ClientCapFlags,
			lb = LB,
			pool_size = PoolSize,
			conn_props = ConnProps,
			conn_pool = ConnPool
		},
	ok = proc_lib:init_ack({ok, self()}),
	{ok, S1} = init_start_all_workers( S0 ),
	gen_server:enter_loop( ?MODULE, [], S1, ?hib_timeout ).

handle_call( ?execute( PacketBin ), GenReplyTo, State ) when is_binary( PacketBin ) ->
	handle_call_execute( PacketBin, GenReplyTo, State );

handle_call( ?shutdown( Reason ), GenReplyTo, State ) ->
	handle_call_shutdown( Reason, GenReplyTo, State );

handle_call(Request, From, State = #s{}) ->
	log_report( warning, [
			?MODULE, handle_call,
			{bad_call, Request},
			{from, From}
		]),
	{reply, {badarg, Request}, State, ?hib_timeout}.

handle_cast(Request, State = #s{}) ->
	log_report( warning, [
				?MODULE, handle_cast,
				{bad_cast, Request}
			]),
	{noreply, State, ?hib_timeout}.

handle_info( timeout, State ) ->
	handle_info_timeout( State );

handle_info( ?orca_packet( WorkerPid, PacketBin ), State ) ->
	case worker_state( WorkerPid ) of
		expect_handshake -> handle_info_packet_handshake( WorkerPid, PacketBin, State );
		expect_auth_result -> handle_info_packet_auth_result( WorkerPid, PacketBin, State );
		expect_init_db_result -> handle_info_init_db_result( WorkerPid, PacketBin, State );
		{ready, DecodeCtx} -> handle_info_packet_generic( WorkerPid, PacketBin, DecodeCtx, State );
		WState ->
			log_report( warning, [
					?MODULE, handle_info,
					{unexpected_packet, PacketBin},
					{from, WorkerPid}, {with_wstate, WState}
				]),
			{noreply, State, ?hib_timeout}
	end;

handle_info( ?worker_start(Idx), State ) ->
	handle_info_worker_start( Idx, State );

handle_info( {'DOWN', Ref, process, Pid, Reason}, State = #s{} ) ->
	handle_info_down( Ref, Pid, Reason, State );

handle_info( Message, State = #s{} ) ->
	log_report( warning, [
				?MODULE, handle_info,
				{bad_info, Message}
			]),
	{noreply, State, ?hib_timeout}.

terminate(_Reason, _State) ->
	ignore.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% %%%%%%%% %%%
%%% Internal %%%
%%% %%%%%%%% %%%

init_conn_pool( PoolSize, MinRestartInterval, Host, Port, ConnOpts0 ) ->
	MgrPid = self(),
	ConnOpts1 = [
			{host, Host}, {port, Port},
			{active, once}, {controlling_process, MgrPid}
		| ConnOpts0 ],

	{ok, Sup} = simplest_one_for_one:start_link( {orca_conn_srv, start_link, [ ConnOpts1 ]} ),
	Pool = #conn_pool{
			sup = Sup,
			workers = [
					#pool_worker{ idx = Idx }
					|| Idx <- lists:seq(0, PoolSize - 1)
				],
			min_restart_interval = MinRestartInterval
		},
	{ok, Pool}.

init_start_all_workers( S0 = #s{ conn_pool = ConnPoolIn } ) ->
	ConnPoolOut = lists:foldl(
		fun ( #pool_worker{ idx = Idx, pid = undefined, mon = undefined }, ConnPool0 ) ->
			{ok, ConnPool1} = conn_pool_worker_start( Idx, ConnPool0 ),
			ConnPool1
		end,
		ConnPoolIn, ConnPoolIn #conn_pool.workers ),
	{ok, S0 #s{ conn_pool = ConnPoolOut }}.

handle_call_shutdown( Reason, _GenReplyTo, State = #s{ conn_pool = #conn_pool{ sup = Sup } } ) ->
	true = erlang:exit( Sup, {shutdown, Reason} ),
	{stop, Reason, ok, State}.

handle_info_packet_handshake(
		WorkerPid, HandshakeReqBin,
		State = #s{
				client_cap_flags = ClientCapFlags,
				conn_props = #conn_props{ user = User, password = Password, database = Database }
			} ) ->
	{ok, {handshake_request, HandshakeReqProps}} = orca_decoder_handshake:decode( HandshakeReqBin ),
	{ok, HandshakeRespBin} = orca_encoder_handshake_response:auth(
		User, Password, Database,
		ClientCapFlags, [
			{client, orca},
			{pid, pid_to_list(WorkerPid)}
		], HandshakeReqProps ),
	ok = orca_conn_srv:send_packet( WorkerPid, 1, HandshakeRespBin ),
	ok = orca_conn_srv:set_active( WorkerPid, once ),
	ok = worker_state( WorkerPid, expect_auth_result ),
	{noreply, State, ?hib_timeout}.

handle_info_packet_auth_result(
	WorkerPid, AuthResultBin,
	State = #s{ conn_props = #conn_props{ database = DatabaseName } }
) ->
	{ok, {AuthResultType, AuthResultProps}} = orca_decoder_generic_response:decode( AuthResultBin ),
	case AuthResultType of
		ok_packet ->
			{ok, COMInitDb} = orca_encoder_com:com_init_db( DatabaseName ),
			ok = orca_conn_srv:send_packet( WorkerPid, 0, COMInitDb ),
			ok = orca_conn_srv:set_active( WorkerPid, once ),
			ok = worker_state( WorkerPid, expect_init_db_result ),
			{noreply, State, ?hib_timeout};
		err_packet ->
			ok = orca_conn_srv:set_active( WorkerPid, once ),
			ok = worker_state( WorkerPid, expect_closed ),

			ok = log_report( warning, [
					?MODULE, handle_info_packet_auth_result, auth_failure
					| AuthResultProps
				]),
			{noreply, State, ?hib_timeout}
	end.

handle_info_init_db_result( WorkerPid, PacketBin, State = #s{ lb = LB0 } ) ->
	{ok, {InitDbResultType, InitDbResultProps}} = orca_decoder_generic_response:decode( PacketBin ),
	case InitDbResultType of
		ok_packet ->
			ok = orca_conn_srv:set_active( WorkerPid, once ),
			{ok, LB1} = orca_conn_mgr_lb:add( WorkerPid, LB0 ),
			ok = worker_state( WorkerPid, {ready, undefined} ),

			{noreply, State #s{ lb = LB1 }, ?hib_timeout};
		err_packet ->
			{ok, ComQuit} = orca_encoder_com:com_quit(),
			ok = orca_conn_srv:send_packet( WorkerPid, 0, ComQuit ),

			ok = orca_conn_srv:set_active( WorkerPid, once ),
			ok = worker_state( WorkerPid, expect_closed ),

			ok = log_report( warning, [
					?MODULE, handle_info_init_db_result, init_db_failure
					| InitDbResultProps
				]),
			{noreply, State, ?hib_timeout}
	end.

handle_info_worker_start( Idx, State = #s{ conn_pool = ConnPool0 } ) ->
	{ok, ConnPool1} = maybe_restart_worker( Idx, ConnPool0 ),
	{noreply, State #s{ conn_pool = ConnPool1 }}.

handle_info_down( Ref, Pid, Reason, State0 = #s{ conn_pool = ConnPool0, lb = LB0 } ) ->
	Workers0 = ConnPool0 #conn_pool.workers,
	case lists:keytake( Ref, #pool_worker.mon, Workers0 ) of
		false ->
			ok = log_report( warning, [
					?MODULE, handle_info_down,
					unexpected_down_msg, {pid, Pid}, {reason, Reason}
				]),
			{noreply, State0, ?hib_timeout};
		{value, W0 = #pool_worker{ idx = Idx, pid = Pid, mon = Ref }, Workers1} ->
			ok = log_report( warning, [
					?MODULE, handle_info_down,
					worker_down, {idx, Idx}, {pid, Pid}, {reason, Reason}
				]),

			ok = worker_state( Pid, undefined ),
			W1 = W0 #pool_worker{ pid = undefined, mon = undefined },
			ConnPool1 = ConnPool0 #conn_pool{ workers = [ W1 | Workers1 ] },
			{ok, ConnPool2} = maybe_restart_worker( Idx, ConnPool1 ),
			{ok, ReplyToQueue, LB1} = orca_conn_mgr_lb:rm( Pid, LB0 ),
			ok = lists:foreach(
				fun (GenReplyTo) ->
					_ = gen_server:reply( GenReplyTo, {error, {conn_down, Reason}} )
				end,
				queue:to_list( ReplyToQueue ) ),
			{noreply, State0 #s{ conn_pool = ConnPool2, lb = LB1 }, ?hib_timeout}
	end.

handle_call_execute( PacketBin, GenReplyTo, State = #s{ lb = LB0 } ) ->
	% log_report( info, [ ?MODULE, handle_call_execute,
	% 	{packet_out, PacketBin}, {gen_reply_to, GenReplyTo} ] ),
	case orca_conn_mgr_lb:job_in( GenReplyTo, LB0 ) of
		{error, Reason} ->
			log_report( warning, [ ?MODULE, handle_call_execute, {lb_error, Reason} ] ),
			{reply, {error, {lb, Reason}}, State};
		{ok, Worker, LB1} ->
			log_report( info, [ ?MODULE, handle_call_execute, {worker, Worker} ] ),
			ok = orca_conn_srv:send_packet( Worker, 0, PacketBin ),
			ok = orca_conn_srv:set_active( Worker, once ),
			% ok = worker_state( Worker, {ready, undefined} ),
			{noreply, State #s{ lb = LB1 }}
	end.

handle_info_packet_generic( WorkerPid, PacketBin, DecodeCtx0, State = #s{ lb = LB0 } ) ->
	% log_report( info, [ ?MODULE, handle_info_packet_generic,
	%	{worker_pid, WorkerPid}, {packet_in, PacketBin} ] ),
	DecodeResult =
		orca_decoder_generic_response:decode_continue( PacketBin, DecodeCtx0 ),
	log_report( info, [ ?MODULE, handle_info_packet_generic,
		{worker_pid, WorkerPid}, {decode_result, DecodeResult} ] ),
	case DecodeResult of
		{incomplete, DecodeCtx1} ->
			ok = worker_state( WorkerPid, {ready, DecodeCtx1} ),
			ok = orca_conn_srv:set_active( WorkerPid, once ),
			{noreply, State};
		{ok, Complete} ->
			{ok, GenReplyTo, LB1} = orca_conn_mgr_lb:job_out( WorkerPid, LB0 ),
			_ = gen_server:reply( GenReplyTo, {ok, Complete} ),
			ok = worker_state( WorkerPid, {ready, undefined} ),
			ok = orca_conn_srv:set_active( WorkerPid, once ),
			{noreply, State #s{ lb = LB1 }}
	end.

handle_info_timeout( State ) ->
	{noreply, State, hibernate}.

maybe_restart_worker( Idx, ConnPool0 ) ->
	case restart_frequency_exceeded( Idx, ConnPool0 ) of
		false ->
			{ok, _ConnPool1} = conn_pool_worker_start( Idx, ConnPool0 );
		{true, AllowedToStartInMs} ->
			_ = erlang:send_after( AllowedToStartInMs, self(), ?worker_start(Idx) ),
			{ok, ConnPool0}
	end.

restart_frequency_exceeded( Idx, #conn_pool{ min_restart_interval = MinRestartInterval, workers = Ws0 } ) ->
	#pool_worker{ last_start = LastStart } = lists:keyfind( Idx, #pool_worker.idx, Ws0 ),
	NowMs = now_ms(),
	case LastStart + MinRestartInterval - NowMs of
		NonPosValue when is_integer(NonPosValue) andalso NonPosValue =< 0 -> false;
		PosValue when is_integer(PosValue) andalso PosValue > 0 -> {true, PosValue}
	end.


now_ms() ->
	{MegS, S, MuS} = erlang:now(),
	(((MegS * 1000000) + S) * 1000) + (MuS div 1000).

conn_pool_worker_start( Idx, ConnPool0 = #conn_pool{ workers = Workers0, sup = Sup } ) ->
	case lists:keytake( Idx, #pool_worker.idx, Workers0 ) of
		{value, W0 = #pool_worker{ pid = undefined, mon = undefined }, Workers1} ->
			case supervisor:start_child( Sup, [] ) of
				{ok, WorkerPid} ->
					WorkerMon = erlang:monitor( process, WorkerPid ),
					W1 = W0 #pool_worker{ idx = Idx, pid = WorkerPid, mon = WorkerMon, last_start = now_ms() },
					ok = worker_state( WorkerPid, expect_handshake ),

					{ok, ConnPool0 #conn_pool{ workers = [ W1 | Workers1 ] }};
				{error, StartChildError} ->
					log_report( warning, [?MODULE, conn_pool_worker_start,
						{idx, Idx}, {failed_to_start_worker, StartChildError} ]),
					W1 = W0 #pool_worker{ last_start = now_ms() },
					ConnPool1 = ConnPool0 #conn_pool{ workers = [ W1 | Workers1 ] },
					{ok, _ConnPool2} = maybe_restart_worker( Idx, ConnPool1 )
			end;
		{value, #pool_worker{ pid = WorkerPid }, _Workers1} when is_pid( WorkerPid ) ->
			{ok, ConnPool0}
	end.

worker_state( WorkerPid, undefined ) -> _ = erlang:erase( {worker_state, WorkerPid} ), ok;
worker_state( WorkerPid, WState ) -> _ = erlang:put( {worker_state, WorkerPid}, WState ), ok.
worker_state( WorkerPid ) -> erlang:get( {worker_state, WorkerPid} ).

log_report( Lvl, Report ) when in( Lvl, [ info, warning, error ] ) ->
	ok = ( erlang:get( ?callback_log ) ) ( Lvl, Report ).

