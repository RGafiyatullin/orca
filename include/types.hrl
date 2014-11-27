-ifndef(orca_include_types_hrl).
-define(orca_include_types_hrl, true).

-type posix_error() :: inet:posix().

-type inet_host() :: inet:ip_address() | inet:hostname().
-type inet_port() :: inet:port_number().
-define(is_inet_port(P), ( is_integer(P) andalso P >= 0 andalso P < 16#FFFF )).

-type db_url() :: string() | binary().

-type log_level() :: info | warning | error.
-type log_report() :: [ term() ].
-type conn_opt() ::
	  {host, inet_host()}
	| {port, inet_port()}
	| {cap_add, pos_integer()} | {cap_rm, pos_integer()}
	| {callback_log, fun( ( log_level(), log_report() ) -> ok )}
	| {callback_log_tcp, fun( ( pid(), in | out, binary() ) -> ok )}
	| {controlling_process, pid()}
	| {socket, port()}
	| {active, once | true | false}.

-endif. % orca_include_types_hrl
