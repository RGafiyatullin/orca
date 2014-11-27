-module (orca_tcp).
-compile ({parse_transform, gin}).
-export ([
		open/2, from_socket/1,
		activate/1,
		send/2,
		messages/1
	]).
-include ("types.hrl").
-record( orca_tcp, {
		sock :: port()
	} ).
-type conn() :: #orca_tcp{}.
-export_type([ conn/0 ]).

-spec open( inet_host(), inet_port() ) -> { ok, conn() } | {error, Reason :: term()}.
-spec activate( conn() ) -> ok | {error, posix_error()}.
-spec send( conn(), binary() | iolist() ) -> ok | {error, posix_error()}.
-spec messages( conn() ) -> {atom(), atom(), atom(), term()}.

from_socket( Sock ) when is_port( Sock ) -> {ok, #orca_tcp{ sock = Sock }}.

open( Host, Port ) when ?is_inet_port( Port ) ->
	case gen_tcp:connect( Host, Port, [ binary, {packet, raw}, {active, false} ] ) of
		{ok, Sock} ->
			{ok, #orca_tcp{
					sock = Sock
				}};
		{error, Reason} -> {error, Reason}
	end.

activate( #orca_tcp{ sock = Sock } ) -> inet:setopts( Sock, [ {active, once} ] ).
send( #orca_tcp{ sock = Sock }, Data ) -> gen_tcp:send( Sock, Data ).
messages( #orca_tcp{ sock = Sock } ) -> {tcp, tcp_closed, tcp_error, Sock}.
