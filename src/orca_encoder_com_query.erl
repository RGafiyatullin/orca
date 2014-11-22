-module (orca_encoder_com_query).
-compile ({parse_transform, gin}).

-export ([
		encode/1,
		encode/2
	]).
-include("proto_consts.hrl").

encode( Query ) when is_binary(Query) orelse is_list(Query) ->
	QueryBin = ensure_binary( Query ),
	{ok, <<?COM_QUERY:8/integer, QueryBin/binary>>}.

encode( Query, Args ) when is_list( Args ) andalso (is_binary(Query) orelse is_list(Query)) ->
	{ok, QueryRendered} = orca_sql_query:render( Query, Args ),
	encode( QueryRendered ).



ensure_binary( B ) when is_binary( B ) -> B;
ensure_binary( L ) when is_list( L ) -> iolist_to_binary( L ).
