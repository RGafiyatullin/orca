-module (orca_conn_mgr).
-compile ({parse_transform, gin}).
-behaviour (gen_server).

-export ([start_link/1]).
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

-define(hib_timeout, 5000).
-define(is_timeout( Timeout ), ( Timeout == infinity orelse (is_integer(Timeout) andalso Timeout > 0) ) ).
-define(execute( PacketBin ), {execute, PacketBin}).

-define(
	init_args( User, Password, Host, Port, Database, PoolSize ),
	{init_args, User, Password, Host, Port, Database, PoolSize} ).
-define(
	orca_packet( ConnSrv, PacketBin ),
	{orca_packet, ConnSrv, PacketBin} ).

start_link( Url ) when is_binary( Url ) ->
	start_link( binary_to_list( Url ) );
start_link( Url = [ $m, $y, $s, $q, $l, $:, $/, $/ | _ ] ) ->
	{ok, {mysql, UserPassword, Host, Port, [ $/ | Database ], MaybeQueryString}} =
		http_uri:parse(Url, [{scheme_defaults, [{mysql, 3306}]}]),
	QueryString =
		case MaybeQueryString of
			[ $? | QS ] -> QS;
			[] -> []
		end,
	ArgsParsed =
		[ begin [K, V] = string:tokens( KV, "=" ), {K, V} end
			|| KV <- string:tokens( QueryString, "&" ) ],
	PoolSize = list_to_integer( proplists:get_value( "pool_size", ArgsParsed, "1" ) ),
	[User, Password] = string:tokens( UserPassword, ":" ),
	InitArgs = ?init_args(
					list_to_binary(User),
					list_to_binary(Password),
					Host, Port,
					list_to_binary(Database),
					PoolSize ),
	proc_lib:start_link( ?MODULE, enter_loop, [InitArgs] ).

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
		mon :: undefined | reference()
	}).
-record(conn_pool, {
		sup :: pid(),
		workers = [] :: [ #pool_worker{} ]
	}).
-record(s, {
		lb :: orca_conn_mgr_lb:ctx(),
		pool_size :: pos_integer(),
		conn_props :: #conn_props{},
		conn_pool :: #conn_pool{}
	}).

init( _ ) -> {error, enter_loop_used}.
enter_loop(?init_args( User, Password, Host, Port, Database, PoolSize )) ->
	ConnProps = #conn_props{
			user = User,
			password = Password,
			host = Host,
			port = Port,
			database = Database
		},
	{ok, ConnPool} = init_conn_pool( PoolSize, Host, Port ),
	{ok, LB} = orca_conn_mgr_lb:new(),
	S0 = #s{
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

handle_call(Request, From, State = #s{}) ->
	error_logger:warning_report([
			?MODULE, handle_call,
			{bad_call, Request},
			{from, From}
		]),
	{reply, {badarg, Request}, State, ?hib_timeout}.

handle_cast(Request, State = #s{}) ->
	error_logger:warning_report([
				?MODULE, handle_cast,
				{bad_cast, Request}
			]),
	{noreply, State, ?hib_timeout}.

handle_info( timeout, State ) ->
	handle_info_timeout( State );

handle_info( ?orca_packet( WorkerPid, PacketBin ), State ) ->
	case worker_state( WorkerPid ) of
		expect_handshake -> handle_info_orca_packet_handshake( WorkerPid, PacketBin, State );
		expect_auth_result -> handle_info_orca_auth_result( WorkerPid, PacketBin, State );
		{ready, DecodeCtx} -> handle_info_orca_packet_generic( WorkerPid, PacketBin, DecodeCtx, State );
		WState ->
			error_logger:warning_report([
					?MODULE, handle_info,
					{unexpected_packet, PacketBin},
					{from, WorkerPid}, {with_wstate, WState}
				]),
			{noreply, State, ?hib_timeout}
	end;

handle_info( {'DOWN', Ref, process, Pid, Reason}, State = #s{} ) ->
	handle_info_down( Ref, Pid, Reason, State );

handle_info( Message, State = #s{} ) ->
	error_logger:warning_report([
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

init_conn_pool( PoolSize, Host, Port ) ->
	{ok, Sup} = simplest_one_for_one:start_link( {orca_conn_srv, start_link, [ Host, Port ]} ),
	Pool = #conn_pool{
			sup = Sup,
			workers = [
					#pool_worker{ idx = Idx }
					|| Idx <- lists:seq(0, PoolSize - 1)
				]
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

handle_info_orca_packet_handshake(
		WorkerPid, HandshakeReqBin,
		State = #s{
				conn_props = #conn_props{ user = User, password = Password, database = Database }
			} ) ->
	{ok, {handshake_request, HandshakeReqProps}} = orca_decoder_handshake:decode( HandshakeReqBin ),
	{ok, HandshakeRespBin} = orca_encoder_handshake_response:auth(
		User, Password, Database, [
			{client, orca},
			{pid, pid_to_list(WorkerPid)}
		], HandshakeReqProps ),
	ok = orca_conn_srv:send_packet( WorkerPid, 1, HandshakeRespBin ),
	ok = orca_conn_srv:set_active( WorkerPid, once ),
	ok = worker_state( WorkerPid, expect_auth_result ),
	{noreply, State, ?hib_timeout}.

handle_info_orca_auth_result( WorkerPid, AuthResultBin, State = #s{ lb = LB0 } ) ->
	{ok, {AuthResultType, AuthResultProps}} = orca_decoder_generic_response:decode( AuthResultBin ),
	case AuthResultType of
		ok_packet ->
			ok = worker_state( WorkerPid, sleep ),
			ok = orca_conn_srv:set_active( WorkerPid, once ),
			{ok, LB1} = orca_conn_mgr_lb:add( WorkerPid, LB0 ),
			{noreply, State #s{ lb = LB1 }, ?hib_timeout};
		err_packet ->
			ok = worker_state( WorkerPid, expect_closed ),
			ok = orca_conn_srv:set_active( WorkerPid, once ),
			ok = error_logger:warning_report([
					?MODULE, handle_info_orca_auth_result, auth_failure
					| AuthResultProps
				]),
			{noreply, State, ?hib_timeout}
	end.

handle_info_down( Ref, Pid, Reason, State0 = #s{ conn_pool = ConnPool0, lb = LB0 } ) ->
	Workers0 = ConnPool0 #conn_pool.workers,
	case lists:keytake( Ref, #pool_worker.mon, Workers0 ) of
		false ->
			ok = error_logger:warning_report([
					?MODULE, handle_info_down,
					unexpected_down_msg, {pid, Pid}, {reason, Reason}
				]),
			{noreply, State0, ?hib_timeout};
		{value, #pool_worker{ idx = Idx, pid = Pid, mon = Ref }, Workers1} ->
			ok = error_logger:warning_report([
					?MODULE, handle_info_down,
					worker_down, {idx, Idx}, {pid, Pid}, {reason, Reason}
				]),

			ok = worker_state( Pid, undefined ),
			ConnPool1 = ConnPool0 #conn_pool{ workers = [ #pool_worker{ idx = Idx } | Workers1] },
			{ok, ConnPool2} = conn_pool_worker_start( Idx, ConnPool1 ),
			{ok, LB1} = orca_conn_mgr_lb:rm( Pid, LB0 ),
			{noreply, State0 #s{ conn_pool = ConnPool2, lb = LB1 }, ?hib_timeout}
	end.

handle_call_execute( PacketBin, GenReplyTo, State = #s{ lb = LB0 } ) ->
	case orca_conn_mgr_lb:job_in( GenReplyTo, LB0 ) of
		{error, Reason} -> {reply, {error, {lb, Reason}}, State};
		{ok, Worker, LB1} ->
			ok = orca_conn_srv:send_packet( Worker, 0, PacketBin ),
			ok = orca_conn_srv:set_active( Worker, once ),
			ok = worker_state( Worker, {ready, undefined} ),
			{noreply, State #s{ lb = LB1 }}
	end.

handle_info_orca_packet_generic( WorkerPid, PacketBin, DecodeCtx0, State = #s{ lb = LB0 } ) ->
	DecodeResult =
		orca_decoder_generic_response:decode_continue( PacketBin, DecodeCtx0 ),
	case DecodeResult of
		{incomplete, DecodeCtx1} ->
			ok = worker_state( WorkerPid, {ready, DecodeCtx1} ),
			ok = orca_conn_srv:set_active( WorkerPid, once ),
			{noreply, State};
		{ok, Complete} ->
			{ok, GenReplyTo, LB1} = orca_conn_mgr_lb:job_out( WorkerPid, LB0 ),
			_ = gen_server:reply( GenReplyTo, {ok, Complete} ),
			ok = worker_state( WorkerPid, {ready, undefined} ),
			{noreply, State #s{ lb = LB1 }}
	end.





handle_info_timeout( State ) ->
	{noreply, State, hibernate}.

conn_pool_worker_start( Idx, ConnPool0 = #conn_pool{ workers = Workers0, sup = Sup } ) ->
	MgrPid = self(),
	ConnInitialOpts = [ {active, once}, {controlling_process, MgrPid} ],
	case lists:keytake( Idx, #pool_worker.idx, Workers0 ) of
		{value, #pool_worker{ pid = undefined, mon = undefined }, Workers1} ->
			{ok, WorkerPid} = supervisor:start_child( Sup, [ ConnInitialOpts ] ),
			WorkerMon = erlang:monitor( process, WorkerPid ),
			WorkerEntry = #pool_worker{ idx = Idx, pid = WorkerPid, mon = WorkerMon },
			ok = worker_state( WorkerPid, expect_handshake ),

			{ok, ConnPool0 #conn_pool{ workers = [ WorkerEntry | Workers1 ] }};
		{value, #pool_worker{ pid = WorkerPid }} when is_pid( WorkerPid ) ->
			{ok, ConnPool0}
	end.

worker_state( WorkerPid, undefined ) -> _ = erlang:erase( {worker_state, WorkerPid} ), ok;
worker_state( WorkerPid, WState ) -> _ = erlang:put( {worker_state, WorkerPid}, WState ), ok.
worker_state( WorkerPid ) -> erlang:get( {worker_state, WorkerPid} ).



