-module (orca_response).
-compile ({parse_transform, gin}).
-export ([
		new/0,
		data_in/2,
		get_packet/1
	]).

-type buffer() :: iolist() | binary().
-record(expect_packet_len, {
		expected_seq_num = 0 :: 0..255,
		prev_payload = <<>> :: buffer()
	}).
-record(expect_seq_num, {
		expected_seq_num :: 0..255,
		expected_payload_size :: non_neg_integer(),
		prev_payload :: buffer()
	}).
-record(reading_data, {
		payload_acc :: buffer(),
		bytes_left_to_read :: non_neg_integer(),
		next_seq_num :: undefined | 0..255
	}).
-type fsm() :: #expect_seq_num{} | #expect_packet_len{} | #reading_data{}.
-record(orca_response_ctx, {
		buf = <<>> :: buffer(),
		fsm = #expect_packet_len{ expected_seq_num = 0, prev_payload = <<>> } :: fsm(),
		packet_q = queue:new() :: queue:queue( binary() )
	}).

-type ctx() :: #orca_response_ctx{}.
-export_type([ ctx/0 ]).

-spec new() -> ctx().
new() -> #orca_response_ctx{}.

-spec data_in( iolist() | binary(), ctx() ) -> {ok, ctx()} | {error, Reason :: term()}.
data_in( IOLOrBin, Ctx0 = #orca_response_ctx{ buf = Buf0, fsm = Fsm0, packet_q = Q0 } ) ->
	Bin = ensure_binary( IOLOrBin ),
	Buf1 = buff_append( Buf0, Bin ),
	case fsm_read_buf( Buf1, Fsm0, Q0 ) of
		{error, Reason} -> {error, Reason};
		{ok, Buf2, Q1, Fsm1} ->
			Ctx1 = Ctx0 #orca_response_ctx{
					packet_q = Q1,
					fsm = Fsm1,
					buf = Buf2
				},
			{ok, Ctx1}
	end.

-spec get_packet( ctx() ) -> {ok, binary(), ctx()} | {error, not_ready}.
get_packet( Ctx0 = #orca_response_ctx{ packet_q = Q0 } ) ->
	case queue:peek( Q0 ) of
		empty -> {error, not_ready};
		{value, Packet} ->
			Q1 = queue:drop( Q0 ),
			Ctx1 = Ctx0 #orca_response_ctx{ packet_q = Q1 },
			{ok, Packet, Ctx1}
	end.


fsm_read_buf( <<>>, State, Q ) -> {ok, <<>>, Q, State};
fsm_read_buf(
	<< PacketLen:24/little, RestOfBuf/binary >>,
	#expect_packet_len{ expected_seq_num = ExpSeqNum, prev_payload = PrevPayload },
	Q
) ->
	NextState = #expect_seq_num{
		expected_seq_num = ExpSeqNum,
		prev_payload = PrevPayload,
		expected_payload_size = PacketLen
	},
	fsm_read_buf( RestOfBuf, NextState, Q );

fsm_read_buf( BufToLittle, State = #expect_packet_len{}, Q ) ->
	{ok, BufToLittle, Q, State};

% fsm_read_buf(
% 	<<ProvidedSeqNum:8/integer, _/binary>>,
% 	#expect_seq_num{
% 		expected_seq_num = ExpSeqNum
% 	},
% 	_Q
% ) when ProvidedSeqNum /= ExpSeqNum ->
% 	{error, {invalid_seq_num, ProvidedSeqNum, ExpSeqNum}};
fsm_read_buf(
	<<_SomeSeqNum:8/integer, RestOfBuf/binary>>,
	#expect_seq_num{
		expected_seq_num = SeqNum,
		prev_payload = PrevPayload,
		expected_payload_size = ExpPayloadSize
	},
	Q
) ->
	NextSeqNum =
		case ExpPayloadSize == 16#FFFFFF of
			false -> undefined;
			true -> cap_ff(SeqNum + 1)
		end,
	NextState = #reading_data{
			bytes_left_to_read = ExpPayloadSize,
			payload_acc = PrevPayload,
			next_seq_num = NextSeqNum
		},
	fsm_read_buf( RestOfBuf, NextState, Q );

fsm_read_buf( BufToLittle, State = #expect_seq_num{}, Q ) ->
	{ok, BufToLittle, Q, State};

fsm_read_buf(
	Buf0,
	State = #reading_data{
		bytes_left_to_read = BytesLeftToRead0,
		payload_acc = PayloadAcc,
		next_seq_num = NextSeqNum
	},
	Q0
) ->
	case Buf0 of
		<<RestOfPayload:BytesLeftToRead0/binary, RestOfBuf/binary>> ->
			PayloadAlreadyRead = [ PayloadAcc, RestOfPayload ],
			{Q1, NextState} =
				case NextSeqNum of
					undefined ->
						{
							queue:in( iolist_to_binary( PayloadAlreadyRead ), Q0 ),
							#expect_packet_len{ expected_seq_num = 0, prev_payload = <<>> }
						};
					_Defined ->
						{
							Q0,
							#expect_packet_len{ expected_seq_num = NextSeqNum, prev_payload = PayloadAlreadyRead }
						}
				end,
			fsm_read_buf( RestOfBuf, NextState, Q1 );
		<<SomeOfPayload/binary>> ->
			BytesLeftToRead1 = BytesLeftToRead0 - size( SomeOfPayload ),
			case BytesLeftToRead1 > 0 of
				true ->
					NextState = State #reading_data{ bytes_left_to_read = BytesLeftToRead1, payload_acc = [ PayloadAcc, SomeOfPayload ] },
					{ok, <<>>, Q0, NextState};
				false ->
					{error, negative_bytes_to_read_value}
			end
	end.


buff_append( <<>>, B ) when is_binary( B ) -> B;
buff_append( B, <<>> ) when is_binary( B ) -> B;
buff_append( BLeft, BRight )
	when is_binary( BLeft ) andalso is_binary( BRight )
->
	<< BLeft/binary, BRight/binary >>.


ensure_binary( Bin ) when is_binary( Bin ) -> Bin;
ensure_binary( IOL ) when is_list( IOL ) -> iolist_to_binary( IOL ).

cap_ff( 16#100 ) -> 0;
cap_ff( I ) when is_integer(I) andalso I < 16#100 -> I.
