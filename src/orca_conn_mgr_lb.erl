-module (orca_conn_mgr_lb).
-compile ({parse_transform, gin}).
-export ([
		new/0,
		add/2,
		rm/2,
		job_in/2,
		job_out/2
	]).
-export_type([ ctx/0 ]).

-type gen_reply_to() :: {pid(), reference()}.
-record(worker, {
		pid :: pid(),
		reply_to_queue = queue:new() :: queue:queue( gen_reply_to() ),
		reply_to_queue_size = 0 :: non_neg_integer()
	}).
-record(ctx, {
		workers = [] :: [ #worker{} ]
	}).
-opaque ctx() :: #ctx{}.

-spec new() -> {ok, ctx()}.
-spec add( pid(), ctx() ) -> {ok, ctx()}.
-spec rm( pid(), ctx() ) -> {ok, queue:queue( gen_reply_to() ), ctx()}.
-spec job_in( gen_reply_to(), ctx() ) -> {ok, pid(), ctx()} | {error, no_workers}.
-spec job_out( pid(), ctx() ) -> {ok, gen_reply_to(), ctx()}.

new() -> {ok, #ctx{}}.

add( Pid, Ctx = #ctx{ workers = Ws0 } ) when is_pid( Pid ) ->
	Ws1 = [ #worker{ pid = Pid } | Ws0 ],
	{ok, Ctx #ctx{ workers = Ws1 }}.

rm( Pid, Ctx = #ctx{ workers = Ws0 } ) when is_pid( Pid ) ->
	{value, #worker{ reply_to_queue = ReplyToQueue }, Ws1} =
		lists:keytake( Pid, #worker.pid, Ws0 ),
	{ok, ReplyToQueue, Ctx #ctx{ workers = Ws1 }}.

job_in( _, #ctx{ workers = [] } ) -> {error, no_workers};
job_in( GenReplyTo = {_, _}, Ctx = #ctx{ workers = [ W0 | Ws0 ] } ) ->
	{ok, W1} = worker_append_job( GenReplyTo, W0 ),
	{ok, Ws1} = worker_insert( W1, Ws0 ),
	{ok, W1 #worker.pid, Ctx #ctx{ workers = Ws1 }}.

job_out( Pid, Ctx = #ctx{ workers = Ws0 } ) ->
	{value, W0, Ws1} = lists:keytake( Pid, #worker.pid, Ws0 ),
	{ok, GenReplyTo, W1} = worker_fetch_job( W0 ),
	{ok, Ws2} = worker_insert( W1, Ws1 ),
	{ok, GenReplyTo, Ctx #ctx{ workers = Ws2 }}.

worker_append_job( GenReplyTo, W0 = #worker{ reply_to_queue = RTQ, reply_to_queue_size = RTQS } ) ->
	W1 = W0 #worker{ reply_to_queue = queue:in( GenReplyTo, RTQ ), reply_to_queue_size = RTQS + 1 },
	{ok, W1}.

worker_fetch_job( W0 = #worker{ reply_to_queue = RTQ, reply_to_queue_size = RTQS } ) ->
	{value, GenReplyTo} = queue:peek( RTQ ),
	{ok, GenReplyTo, W0 #worker{ reply_to_queue = queue:drop( RTQ ), reply_to_queue_size = RTQS - 1 }}.

worker_insert( W = #worker{ reply_to_queue_size = QS }, Ws0 ) ->
	{WsLeft, WsRight} = zip( right,
		fun( _Left, [ #worker{ reply_to_queue_size = WCurrentQS } | _Right ] ) ->
			WCurrentQS < QS
		end, {[], Ws0} ),
	{[], Ws1} = zip( left, fun( _, _ ) -> true end, {WsLeft, [ W | WsRight ]} ),
	{ok, Ws1}.


-spec zip( left | right, fun(( [T], [T] ) -> boolean()), {[T], [T]} ) -> {[T], [T]}.
-spec zip_right_till( fun(( [T], [T] ) -> boolean()), {[T], [T]} ) -> {[T], [T]}.
-spec zip_left_till( fun(( [T], [T] ) -> boolean()), {[T], [T]} ) -> {[T], [T]}.


zip( Direction, F, {L, R} ) ->
	case Direction of
		left -> zip_left_till( F, {L, R} );
		right -> zip_right_till( F, {L, R} )
	end.

zip_right_till( _, {L, []} ) -> {L, []};
zip_right_till( F, {L, CandR = [ C | R ]} ) ->
	case F(L, CandR) of
		false -> {L, CandR};
		true -> zip_right_till( F, {[ C | L ], R} )
	end.

zip_left_till( _, {[], R} ) -> {[], R};
zip_left_till( F, {CandL = [ C | L ], R} ) ->
	case F( CandL, R ) of
		false -> {CandL, R};
		true -> zip_left_till( F, { L, [ C | R ] } )
	end.


