-module (orca_type_decoder).
-compile ({parse_transform, gin}).

-export ([
		take/2,
		take_seq/2
	]).

-type mysql_enc_int_type() :: 1 | 2 | 3 | 4 | 6 | 8 | lenenc.
-type mysql_enc_int() :: {int, mysql_enc_int_type()}.
-type mysql_enc_string_type() :: pos_integer() | null | var | lenenc | eof.
-type mysql_enc_string() :: {string, mysql_enc_string_type()}.
-type mysql_enc_type() ::
		mysql_enc_int() |
		mysql_enc_string().



-spec take(
	Type :: mysql_enc_type(), PacketIn :: binary()
) ->
	{ok, Value :: term(), PacketOut :: binary()}
	| {error, Reason :: term()}.

take_seq( Types, BinIn ) when is_list( Types ) andalso is_binary( BinIn ) ->
	{QFinal, BinOut} = lists:foldl(
		fun( Type, {Q, Bin0} ) ->
			{ok, Value, Bin1} = take( Type, Bin0 ),
			{queue:in( Value, Q ), Bin1}
		end,
		{queue:new(), BinIn}, Types ),
	{ ok, queue:to_list( QFinal ), BinOut }.

take( {int, lenenc}, BinIn ) ->
	case BinIn of
		<<16#FC:8/integer, IntValue:16/little, BinOut/binary>> -> {ok, IntValue, BinOut};
		<<16#FD:8/integer, IntValue:24/little, BinOut/binary>> -> {ok, IntValue, BinOut};
		<<16#FE:8/integer, IntValue:64/little, BinOut/binary>> -> {ok, IntValue, BinOut};
		<< IntValue:8/integer, BinOut/binary >> when IntValue < 16#FB -> {ok, IntValue, BinOut};
		_ -> {error, badarg}
	end;
take( {int, ByteWidth}, BinIn ) when in( ByteWidth, [ 1,2,3,4,6,8 ] ) ->
	BitWidth = ByteWidth * 8,
	case BinIn of
		<< IntValue:BitWidth/little, BinOut/binary >> -> {ok, IntValue, BinOut};
		_ -> {error, badarg}
	end;
take( {string, ByteLen}, BinIn ) when is_integer(ByteLen) andalso ByteLen >= 0 ->
	case BinIn of
		<< StrValue:ByteLen/binary, BinOut/binary >> -> {ok, StrValue, BinOut};
		_ -> {error, badarg}
	end;
take( {string, null}, BinIn ) ->
	case binary:split( BinIn, <<0>> ) of
		[ StrValue, BinOut ] -> {ok, StrValue, BinOut};
		_ -> {error, badarg}
	end;
take( {string, eof}, BinIn ) ->
	{ok, BinIn, <<>>};
take( {string, lenenc}, Bin0 ) ->
	{ok, StrLen, Bin1} = take( {int, lenenc}, Bin0 ),
	{ok, _StrVal, _Bin2} = take( {string, StrLen}, Bin1 );
take( _, _ ) -> not_implemented().


not_implemented() -> {error, not_implemented}.



