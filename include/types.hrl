-ifndef(orca_include_types_hrl).
-define(orca_include_types_hrl, true).

-type posix_error() :: inet:posix().

-type inet_host() :: inet:ip_address() | inet:hostname().
-type inet_port() :: inet:port_number().
-define(is_inet_port(P), ( is_integer(P) andalso P >= 0 andalso P < 16#FFFF )).

-endif. % orca_include_types_hrl
