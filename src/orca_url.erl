-module (orca_url).
-export ([parse/1, render/1]).

-define(default_mysqld_port, 3306).

parse( Url ) when is_binary( Url ) -> parse( binary_to_list( Url ) );
parse( Url ) when is_list( Url ) ->
	try
		{ok, {mysql, UserPassword, Host, Port, [ $/ | Database ], MaybeQueryString}} =
			http_uri:parse(Url, [{scheme_defaults, [{mysql, ?default_mysqld_port}]}]),
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

render(Props) ->
	User = proplists:get_value(user, Props),
	Password = proplists:get_value(password, Props),
	Host = proplists:get_value(host, Props),
	Port = proplists:get_value(port, Props, ?default_mysqld_port),
	DbName = proplists:get_value(db_name, Props),
	PoolSize = proplists:get_value(pool_size, Props, 1),
	MinRestartInterval = proplists:get_value(min_restart_interval, Props, 1000),

	iolist_to_binary([
		<<"mysql://">>,
		User, $:, Password, $@,
		Host, $:, integer_to_list(Port), $/, DbName, $?,
		<<"pool_size=">>, integer_to_list(PoolSize),
		<<"&min_restart_interval=">>, integer_to_list(MinRestartInterval)
	]).

