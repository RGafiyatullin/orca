-module (orca_conn_srv).
-compile ({parse_transform, gin}).
-behaviour (gen_server).

-export ([ start_link/2, start_link/3 ]).
-export ([ set_active/2, send_packet/3 ]).
-export ([
		init/1, enter_loop/3,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
	]).
-include ("types.hrl").

-type option() :: { term(), term() }.

-spec start_link( inet_host(), inet_port() ) -> {ok, pid()} | {error, Reason :: term()}.
-spec start_link( inet_host(), inet_port(), [option()] ) -> {ok, pid()} | {error, Reason :: term()}.

-define(set_active( Mode ), {set_active, Mode}).
-define(send_packet( SeqID, Packet ), {send_packet, SeqID, Packet}).

-define(hib_timeout, 5000).

start_link( Host, Port ) -> start_link( Host, Port, [] ).
start_link( Host, Port, Opts0 ) when ?is_inet_port( Port ) ->
	Opts1 = opts_ensure_controlling_process( Opts0 ),
	proc_lib:start_link( ?MODULE, enter_loop, [ Host, Port, Opts1 ] ).

set_active( Srv, Mode )
	when (is_pid( Srv ) orelse is_atom( Srv ))
	andalso in( Mode, [ true, false, once ] )
->
	ok = gen_server:cast( Srv, ?set_active( Mode ) ).

send_packet( Srv, SeqID, Packet )
	when is_pid( Srv )
	andalso is_integer( SeqID ) andalso SeqID >= 0 andalso SeqID =< 255
	andalso is_binary( Packet )
->
	gen_server:cast( Srv, ?send_packet( SeqID, Packet ) ).


-record(s, {
		host :: inet_host(),
		port :: inet_port(),
		opts :: [ option() ],

		tcp :: orca_tcp:conn(),
		msg_port :: term(),
		msg_data :: atom(),
		msg_closed :: atom(),
		msg_error :: atom(),

		controlling_process :: pid(),
		controlling_process_mon_ref :: reference(),
		response_ctx :: orca_response:ctx(),

		active = false :: false | true | once
	}).

enter_loop( Host, Port, Opts ) ->
	case orca_tcp:open( Host, Port ) of
		{ok, Tcp} ->
			ok = proc_lib:init_ack( {ok, self()} ),
			{controlling_process, ControllingProcess} = lists:keyfind( controlling_process, 1, Opts ),
			InitialActiveMode = proplists:get_value( active, Opts, false ),
			{MsgData, MsgClosed, MsgError, MsgPort} = orca_tcp:messages( Tcp ),
			ResponseCtx = orca_response:new(),
			ControllingProcessMonRef = erlang:monitor( process, ControllingProcess ),
			ok = orca_tcp:activate( Tcp ),
			S0 = #s{
					host = Host,
					port = Port,
					opts = Opts,

					tcp = Tcp,

					msg_data = MsgData,
					msg_closed = MsgClosed,
					msg_error = MsgError,
					msg_port = MsgPort,

					response_ctx = ResponseCtx,

					controlling_process = ControllingProcess,
					controlling_process_mon_ref = ControllingProcessMonRef,
					active = InitialActiveMode
				},
			gen_server:enter_loop( ?MODULE, [], S0, ?hib_timeout );
		ErrorReply = {error, _} ->
			ok = proc_lib:init_ack( ErrorReply ),
			exit(shutdown)
	end.

init( _ ) -> {error, enter_loop_used}.


handle_call(Request, From, State = #s{}) ->
	error_logger:warning_report([
			?MODULE, handle_call,
			{bad_call, Request},
			{from, From}
		]),
	{reply, {badarg, Request}, State, ?hib_timeout}.

handle_cast( ?set_active( Mode ), State = #s{} ) ->
	handle_cast_set_active( Mode, State );

handle_cast( ?send_packet( SeqID, Packet ), State ) ->
	handle_cast_send_packet( SeqID, Packet, State );

handle_cast(Request, State = #s{}) ->
	error_logger:warning_report([
				?MODULE, handle_cast,
				{bad_cast, Request}
			]),
	{noreply, State, ?hib_timeout}.

handle_info( timeout, State ) ->
	handle_info_timeout( State );

handle_info(
		{'DOWN', ControllingProcessMonRef, process, ControllingProcess, Reason},
		State = #s{
			controlling_process = ControllingProcess,
			controlling_process_mon_ref = ControllingProcessMonRef
		}
	) ->
		{stop, {shutdown, {controlling_process_terminated, Reason}}, State};

handle_info( {MsgClosed, MsgPort}, State = #s{ msg_closed = MsgClosed, msg_port = MsgPort } ) ->
	handle_info_closed( State );

handle_info( {MsgData, MsgPort, Data}, State = #s{ msg_data = MsgData, msg_port = MsgPort } ) ->
	handle_info_data( Data, State );

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

opts_ensure_controlling_process( Opts0 ) ->
	_Opts1 =
		case lists:keyfind( controlling_process, 1, Opts0 ) of
			false -> [ {controlling_process, self()} | Opts0 ];
			{controlling_process, _} -> Opts0
		end.

handle_cast_set_active( Mode, State0 = #s{} ) ->
	State1 = State0 #s{ active = Mode },
	{ok, State2} = maybe_deliver_packets( State1 ),
	{noreply, State2, ?hib_timeout}.

handle_info_data( Data, State0 = #s{ response_ctx = ResponseCtx0 } ) ->
	% error_logger:info_report([?MODULE, handle_info_data, {data, Data}]),
	{ok, ResponseCtx1} = orca_response:data_in( Data, ResponseCtx0 ),
	State1 = State0 #s{ response_ctx = ResponseCtx1 },
	{ok, State2} = maybe_deliver_packets( State1 ),
	{noreply, State2, ?hib_timeout}.


handle_info_closed( State0 ) ->
	%% FIXME: deliver all the pending events prior to terminating.
	%% NOTE: the latter must be done regarding the "activeness" of this srv.
	{stop, {shutdown, tcp_closed}, State0}.
	% {noreply, State0}.

handle_cast_send_packet( SeqID, Packet, State = #s{ tcp = Tcp } ) ->
	% error_logger:info_report([?MODULE, handle_cast_send_packet]),
	PacketLen = size(Packet),
	PacketHeader = << PacketLen:24/little, SeqID:8/integer >>,
	ok = orca_tcp:send( Tcp, [PacketHeader, Packet] ),
	{noreply, State, ?hib_timeout}.

handle_info_timeout( State ) ->
	{noreply, State, hibernate}.




maybe_deliver_packets( State0 = #s{ active = false } ) -> {ok, State0};
maybe_deliver_packets(
	State0 = #s{
		active = ShouldSend,
		response_ctx = ResponseCtx0,
		controlling_process = ControllingProcess
	}
) when in( ShouldSend, [ true, once ] ) ->
	case orca_response:get_packet( ResponseCtx0 ) of
		{error, not_ready} ->
			% error_logger:info_report([?MODULE, maybe_deliver_packets, packet_q_empty]),
			maybe_activate_tcp( State0 );
		{ok, Packet, ResponseCtx1} ->
			ok = deliver_packet( ControllingProcess, Packet ),
			NextActiveMode =
				case ShouldSend of
					true -> true;
					once -> false
				end,
			State1 = State0 #s{ active = NextActiveMode, response_ctx = ResponseCtx1 },
			maybe_deliver_packets( State1 )
	end.

maybe_activate_tcp( State0 = #s{ active = false } ) -> {ok, State0};
maybe_activate_tcp( State0 = #s{ active = ShouldActivate, tcp = Tcp } )
	when in( ShouldActivate, [ true, once ] ) ->
		% error_logger:info_report([?MODULE, maybe_activate_tcp]),
		ok = orca_tcp:activate( Tcp ),
		{ok, State0}.



deliver_packet( ControllingProcess, Packet ) when is_pid( ControllingProcess ) andalso is_binary( Packet ) ->
	% error_logger:info_report([?MODULE, deliver_packet, {packet, Packet}]),
	_ = erlang:send( ControllingProcess, {orca_packet, self(), Packet} ),
	ok.
