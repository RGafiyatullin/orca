-module (orca_url).
-export ([parse/1]).

parse( Url ) when is_binary( Url ) -> parse( binary_to_list( Url ) );
parse( Url ) when is_list( Url ) ->
	try
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
		MinRestartInterval = list_to_integer( proplists:get_value( "min_restart_interval", ArgsParsed, "1000" ) ),
		[User, Password] = string:tokens( UserPassword, ":" ),
		{ok, [
				{user, list_to_binary(User)},
				{password, list_to_binary(Password)},
				{host, Host},
				{port, Port},
				{db_name, list_to_binary(Database)},
				{pool_size, PoolSize},
				{min_restart_interval, MinRestartInterval}
			]}
	catch
		Error:Reason -> {Error, Reason}
	end.
