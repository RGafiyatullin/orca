-module (orca_sql_query).
-export ([render/2]).

-spec render( Query :: iolist() | binary(), Args :: [ term() ] ) -> {ok, RenderedQuery :: iolist()}.
render( QueryIOL, Args ) ->
	Query = iolist_to_binary( [QueryIOL] ),
	render( Query, queue:new(), Args ).

-spec render( QuerySoFar :: binary(), QueryRendered :: queue:queue( term() ), Args :: [term()] ) -> {ok, RenderedQuery :: iolist()}.
render( <<>>, _QueryRendered, [ _ | _ ] ) -> {error, too_many_args};
render( QuerySoFar, QueryRendered, [] ) -> {ok, [ queue:to_list( QueryRendered ), QuerySoFar ]};
render( << $?/utf8, QuerySoFar/binary >>, QueryRendered, [ Arg | Args ] ) ->
	ArgL = arg_literal( Arg ),
	render( QuerySoFar, queue:in( ArgL, QueryRendered ), Args );

render( << Quot/utf8, QuerySoFar/binary >>, QueryRendered, Args )
	when Quot == $'
	% orelse Quot == $"
	orelse Quot == $`
->
	case skip_quot( Quot, QuerySoFar, queue:in( Quot, QueryRendered ) ) of
		{ok, QuerySoFar1, QueryRendered1} -> render( QuerySoFar1, QueryRendered1, Args);
		{error, Reason} -> {error, Reason}
	end;

render( << Char/utf8, QuerySoFar/binary >>, QueryRendered, Args ) -> render( QuerySoFar, queue:in( Char, QueryRendered ), Args ).

arg_literal( Val ) -> sql_encode( Val, true ).

skip_quot( Quot, << Quot/utf8, QuerySoFar/binary >>, QueryRendered ) ->	{ ok, QuerySoFar, queue:in( Quot, QueryRendered ) };
skip_quot( Quot, << Char/utf8, QuerySoFar/binary >>, QueryRendered ) -> skip_quot( Quot, QuerySoFar, queue:in( Char, QueryRendered ) );
skip_quot( Quot, <<>>, _QueryRendered ) -> {error, {quot_mismatch, Quot}}.

sql_encode(Val, false) when Val == undefined; Val == null ->
	"null";
sql_encode(Val, true) when Val == undefined; Val == null ->
	<<"null">>;
sql_encode(Val, false) when is_binary(Val) ->
	anybin_to_list(quote(Val));
sql_encode(Val, true) when is_binary(Val) ->
	quote(Val);
sql_encode(Val, true) ->
	unicode:characters_to_binary(sql_encode(Val,false));
sql_encode(Val, false) when is_atom(Val) ->
	quote(atom_to_list(Val));
sql_encode(Val, false) when is_list(Val) ->
	quote(Val);
sql_encode(Val, false) when is_integer(Val) ->
	integer_to_list(Val);
sql_encode(Val, false) when is_float(Val) ->
	[Res] = io_lib:format("~w", [Val]),
	Res;
sql_encode({datetime, Val}, AsBinary) ->
	sql_encode(Val, AsBinary);
sql_encode({{Year, Month, Day}, {Hour, Minute, Second}}, false) ->
	Res = two_digits([Year, Month, Day, Hour, Minute, Second]),
	lists:flatten(Res);
sql_encode({TimeType, Val}, AsBinary) when TimeType == 'date'; TimeType == 'time' ->
	sql_encode(Val, AsBinary);
sql_encode({Time1, Time2, Time3}, false) ->
	Res = two_digits([Time1, Time2, Time3]),
	lists:flatten(Res);
sql_encode(Val, _AsBinary) ->
	{error, {unrecognized_value, Val}}.

two_digits(Nums) when is_list(Nums) ->
	[two_digits(Num) || Num <- Nums];
two_digits(Num) ->
	[Str] = io_lib:format("~b", [Num]),
	case length(Str) of
		1 -> [$0 | Str];
		_ -> Str
	end.

anybin_to_list(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin,utf8,utf8) of
		Bin -> unicode:characters_to_list(Bin);
		_ -> binary_to_list(Bin)
    end.

quote(String) when is_list(String) ->
	[39 | lists:reverse([39 | quote(String, [])])]; %% 39 is $'
quote(Bin) when is_binary(Bin) ->
	list_to_binary(quote(binary_to_list(Bin))).

quote([], Acc) ->
	Acc;
quote([0 | Rest], Acc) ->
	quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
	quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
	quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
	quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) -> %% 39 is $'
	quote(Rest, [39, $\\ | Acc]); %% 39 is $'
quote([34 | Rest], Acc) -> %% 34 is $"
	quote(Rest, [34, $\\ | Acc]); %% 34 is $"
quote([26 | Rest], Acc) ->
	quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
	quote(Rest, [C | Acc]).
