-module (orca_decoder_generic_response).
-compile ({parse_transform, gin}).
-export ([
		decode/1,
		decode_continue/2
	]).
-include("proto_consts.hrl").

-record( expect_field, { fields_rev_acc :: [ binary() ], fields_left :: pos_integer() } ).
-record( expect_eof_before_rows, { field_def_packets :: [ binary() ] } ).
-record( expect_row_or_eof, { field_def_packets :: [ binary() ], rows_rev_acc :: [ binary() ] } ).

-type packet_type() :: ok_packet | err_packet.
-type packet_prop() :: {atom(), term()}.
-type incomplete_ctx() :: #expect_field{} | #expect_eof_before_rows{} | #expect_row_or_eof{}.
-spec decode( binary() ) ->
	  {ok, {packet_type(), [packet_prop()]}}
	| {incomplete, incomplete_ctx()}.

decode( Bin0 ) ->
	{ok, PacketType, Bin1} = orca_type_decoder:take({int, 1}, Bin0),
	case PacketType of
		?PACKET_TYPE_OK -> decode_ok_packet( Bin1 );
		?PACKET_TYPE_ERR -> decode_err_packet( Bin1 );
		% ?PACKET_TYPE_EOF -> decode_eof_packet( Bin1 );
		FieldsCount -> decode_result_set( FieldsCount )
	end.

decode_continue( Bin, undefined ) -> decode( Bin );

decode_continue( Bin, #expect_field{ fields_rev_acc = Acc0, fields_left = 1 } ) ->
	{incomplete, #expect_eof_before_rows{ field_def_packets = lists:reverse( [ Bin | Acc0 ] ) }};
decode_continue( Bin, S = #expect_field{ fields_rev_acc = Acc0, fields_left = Left0 } ) ->
	{incomplete, S #expect_field{ fields_rev_acc = [ Bin | Acc0 ], fields_left = Left0 - 1 }};

decode_continue( <<?PACKET_TYPE_EOF:8/integer, _/binary>>, #expect_eof_before_rows{ field_def_packets = FieldDefPackets } ) ->
	{incomplete, #expect_row_or_eof{ field_def_packets = FieldDefPackets, rows_rev_acc = [] }};

decode_continue(
	<<?PACKET_TYPE_EOF:8/integer, Bin0/binary>>,
	#expect_row_or_eof{ field_def_packets = FieldDefPackets, rows_rev_acc = RevAcc }
) ->
	{ok, NumberOfWarnings, Bin1} = orca_type_decoder:take( {int, 2}, Bin0 ),
	{ok, StatusFlags, _Bin2} = orca_type_decoder:take( {int, 2}, Bin1 ),
	{ok, {result_set_raw, [
			{rows_raw, lists:reverse( RevAcc )},
			{field_defs_raw, FieldDefPackets},
			{status_flags, StatusFlags},
			{number_of_warnings, NumberOfWarnings}
		]}};
decode_continue( Bin, S = #expect_row_or_eof{ rows_rev_acc = Acc0 } ) ->
	{incomplete, S #expect_row_or_eof{ rows_rev_acc = [ Bin | Acc0 ] }}.



decode_ok_packet( Bin0 ) ->
	{ok, AffectedRows, Bin1} = orca_type_decoder:take( {int, lenenc}, Bin0 ),
	{ok, LastInsertedID, Bin2} = orca_type_decoder:take( {int, lenenc}, Bin1 ),
	{ok, StatusFlags, Bin3} = orca_type_decoder:take( {int, 2}, Bin2 ),
	{ok, NumberOfWarnings, Bin4} = orca_type_decoder:take( {int, 2}, Bin3 ),
	{ok, OKMsg, <<>>} = orca_type_decoder:take( {string, eof}, Bin4 ),
	{ok, {ok_packet, [
			{affected_rows, AffectedRows},
			{last_inserted_id, LastInsertedID},
			{status_flags, StatusFlags},
			{number_of_warnings, NumberOfWarnings},
			{ok_msg, OKMsg}
		]}}.

decode_err_packet( Bin0 ) ->
	{ok, ErrCode, Bin1} = orca_type_decoder:take( {int, 2}, Bin0 ),
	{ok, SqlStateMarker, Bin2} = orca_type_decoder:take( {string, 1}, Bin1 ),
	{ok, SqlState, Bin3} = orca_type_decoder:take( {string, 5}, Bin2 ),
	{ok, ErrMsg, <<>>} = orca_type_decoder:take( {string, eof}, Bin3 ),
	{ok, {err_packet, [
			{err_code, ErrCode},
			{err_msg, ErrMsg},
			{sql_state_marker, SqlStateMarker},
			{sql_state, SqlState}
		]}}.

decode_result_set( FieldsCount ) -> {incomplete, #expect_field{ fields_rev_acc = [], fields_left = FieldsCount }}.



