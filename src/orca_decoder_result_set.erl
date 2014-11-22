-module (orca_decoder_result_set).
-compile ({parse_transform, gin}).
-export ([
		decode/1
	]).
-include ("proto_consts.hrl").

decode( RawResultSetProps ) ->
	{field_defs_raw, FieldDefsRaw} = lists:keyfind( field_defs_raw, 1, RawResultSetProps ),

	FieldDefs = lists:map(
			fun decode_field_def/1,
			FieldDefsRaw),
	FieldDefRecords = lists:map(
			fun def_props_to_record/1,
			FieldDefs),

	{rows_raw, RowsRaw} = lists:keyfind( rows_raw, 1, RawResultSetProps ),
	Rows = lists:map(
		fun ( RowRaw ) ->
			decode_row_using_field_defs( FieldDefRecords, RowRaw )
		end,
		RowsRaw),
	{ok, [
			{field_defs, FieldDefs},
			{rows, Rows}
		]}.

decode_field_def( Bin0 ) ->
	{ok, [
			<<"def">>,
			Schema,
			Table,
			OrgTable,
			Name,
			OrgName,
			LenOfFixedLen,
			CharSet,
			ColLen,
			ColType,
			ColFlags,
			Decimals,
			_Filler
		], <<>>} = orca_type_decoder:take_seq([
				{string, lenenc},
				{string, lenenc},
				{string, lenenc},
				{string, lenenc},
				{string, lenenc},
				{string, lenenc},
				{int, lenenc},
				{int, 2},
				{int, 4},
				{int, 1},
				{int, 2},
				{int, 1},
				{int, 2}
			], Bin0),
	[
			{schema, Schema},
			{table, Table},
			{org_table, OrgTable},
			{name, Name},
			{org_name, OrgName},
			{len_of_fixed_len, LenOfFixedLen},
			{charset, CharSet},
			{col_len, ColLen},
			{col_type, ColType},
			{col_flags, ColFlags},
			{decimals, Decimals}
		].

-record(field, {
		col_type :: integer()
	}).
-define(f(FieldName), FieldName = proplists:get_value( FieldName, Props ) ).
def_props_to_record( Props ) ->
	#field{
			?f(col_type)
		}.

decode_row_using_field_defs( FieldDefRecords, RowRaw ) ->
	{AccRev, <<>>} = lists:foldl(
		fun decode_row_using_field_defs_folder/2,
		{[], RowRaw}, FieldDefRecords),
	lists:reverse( AccRev ).



decode_row_using_field_defs_folder( F, {Acc, RowIn} ) ->
	{Value, RowOut} = decode_row_using_field_defs_take( F, RowIn ),
	{[Value | Acc], RowOut}.


decode_row_using_field_defs_take( #field{}, << 16#FB:8/integer, RowOut/binary >> ) -> {undefined, RowOut};
decode_row_using_field_defs_take( #field{ col_type = ColType }, RowIn ) ->
	{ok, Value, RowOut} = orca_type_decoder:take( {string, lenenc}, RowIn ),
	{maybe_parse_value( ColType, Value ), RowOut}.


maybe_parse_value( ?MYSQL_TYPE_DECIMAL, Encoded ) -> b2f( Encoded );
maybe_parse_value( ?MYSQL_TYPE_TINY, Encoded ) -> b2i( Encoded );
maybe_parse_value( ?MYSQL_TYPE_SHORT, Encoded ) -> b2i( Encoded );
maybe_parse_value( ?MYSQL_TYPE_LONG, Encoded ) -> b2i( Encoded );
maybe_parse_value( ?MYSQL_TYPE_FLOAT, Encoded ) -> b2f( Encoded );
maybe_parse_value( ?MYSQL_TYPE_DOUBLE, Encoded ) -> b2f( Encoded );
maybe_parse_value( ?MYSQL_TYPE_NULL, Encoded ) -> {not_ipmlemented, ?MYSQL_TYPE_NULL, Encoded};
maybe_parse_value( ?MYSQL_TYPE_TIMESTAMP, Encoded ) -> b2datetime( Encoded );
maybe_parse_value( ?MYSQL_TYPE_LONGLONG, Encoded ) -> b2i( Encoded );
maybe_parse_value( ?MYSQL_TYPE_INT24, Encoded ) -> b2i( Encoded );
maybe_parse_value( ?MYSQL_TYPE_DATE, Encoded ) -> b2date( Encoded );
maybe_parse_value( ?MYSQL_TYPE_TIME, Encoded ) -> b2time( Encoded );
maybe_parse_value( ?MYSQL_TYPE_DATETIME, Encoded ) -> b2datetime( Encoded );
maybe_parse_value( ?MYSQL_TYPE_YEAR, Encoded ) -> b2i( Encoded );
maybe_parse_value( ?MYSQL_TYPE_NEWDATE, Encoded ) -> {not_ipmlemented, ?MYSQL_TYPE_NEWDATE, Encoded};
maybe_parse_value( ?MYSQL_TYPE_VARCHAR, Encoded ) -> Encoded;
maybe_parse_value( ?MYSQL_TYPE_BIT, Encoded ) -> b2bool( Encoded );
maybe_parse_value( ?MYSQL_TYPE_TIMESTAMP2, Encoded ) -> {not_ipmlemented, ?MYSQL_TYPE_TIMESTAMP2, Encoded};
maybe_parse_value( ?MYSQL_TYPE_DATETIME2, Encoded ) -> {not_ipmlemented, ?MYSQL_TYPE_DATETIME2, Encoded};
maybe_parse_value( ?MYSQL_TYPE_TIME2, Encoded ) -> {not_ipmlemented, ?MYSQL_TYPE_TIME2, Encoded};
maybe_parse_value( ?MYSQL_TYPE_NEWDECIMAL, Encoded ) -> b2f( Encoded );
maybe_parse_value( ?MYSQL_TYPE_ENUM, Encoded ) -> b2_enum_stub( Encoded );
maybe_parse_value( ?MYSQL_TYPE_SET, Encoded ) -> {not_ipmlemented, ?MYSQL_TYPE_SET, Encoded};
maybe_parse_value( ?MYSQL_TYPE_TINY_BLOB, Encoded ) -> Encoded;
maybe_parse_value( ?MYSQL_TYPE_MEDIUM_BLOB, Encoded ) -> Encoded;
maybe_parse_value( ?MYSQL_TYPE_LONG_BLOB, Encoded ) -> Encoded;
maybe_parse_value( ?MYSQL_TYPE_BLOB, Encoded ) -> Encoded;
maybe_parse_value( ?MYSQL_TYPE_VAR_STRING, Encoded ) -> Encoded;
maybe_parse_value( ?MYSQL_TYPE_STRING, Encoded ) -> Encoded;
maybe_parse_value( ?MYSQL_TYPE_GEOMETRY, Encoded ) -> {not_ipmlemented, ?MYSQL_TYPE_GEOMETRY, Encoded};
maybe_parse_value( ColType, Encoded ) -> {not_implemented, ColType, Encoded}.

b2i( B ) -> list_to_integer( binary_to_list( B ), 10 ).

b2bool( <<"0">> ) -> false;
b2bool( <<"1">> ) -> true.

b2_enum_stub( B ) -> B.

%% from emysql_tcp:type_cast_row_data/2
%% * b2f/1
%% * b2date/1
%% * b2time/1
%% * b2datetime/1

b2f( B ) ->
	{ok, [Num], _Leftovers} =
		case io_lib:fread("~f", binary_to_list(B)) of
			% note: does not need conversion
			{error, _} ->
				case io_lib:fread("~d", binary_to_list(B)) of  % note: does not need conversion
					{ok, [_], []} = Res ->
						Res;
					{ok, [X], E} ->
						io_lib:fread("~f", lists:flatten(io_lib:format("~w~s~s" ,[X,".0",E])))
				end;
			Res ->
				Res
		end,
	Num.
b2date( B ) ->
	case io_lib:fread("~d-~d-~d", binary_to_list(B)) of  % note: does not need conversion
		{ok, [Year, Month, Day], _} ->
			{date, {Year, Month, Day}};
		{error, _} ->
			binary_to_list(B);  % todo: test and possibly conversion to UTF-8
		_ ->
			exit({error, bad_date})
	end.
b2time( B ) ->
	case io_lib:fread("~d:~d:~d", binary_to_list(B)) of  % note: does not need conversion
		{ok, [Hour, Minute, Second], _} ->
			{time, {Hour, Minute, Second}};
		{error, _} ->
			binary_to_list(B);  % todo: test and possibly conversion to UTF-8
		_ ->
			exit({error, bad_time})
	end.
b2datetime( B ) ->
	case io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(B)) of % note: does not need conversion
		{ok, [Year, Month, Day, Hour, Minute, Second], _} ->
			{datetime, {{Year, Month, Day}, {Hour, Minute, Second}}};
		{error, _} ->
			binary_to_list(B);   % todo: test and possibly conversion to UTF-8
		_ ->
			exit({error, datetime})
	end.
