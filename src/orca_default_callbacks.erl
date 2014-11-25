-module (orca_default_callbacks).
-compile ({parse_transform, gin}).
-export ([log_error_logger/2, log_null/2]).
-export ([log_tcp_null/3, log_tcp_error_logger/3]).


log_error_logger( LogLevel, Report ) ->
	F =
		case LogLevel of
			info -> info_report;
			warning -> warning_report;
			error -> error_report
		end,
	ok = error_logger:F( Report ).

log_null( _LogLevel, _Report ) -> ok.


log_tcp_null( _Conn, _Direction, _Binary ) -> ok.
log_tcp_error_logger( Conn, Direction, Binary ) when in(Direction, [in, out]) ->
	ok = error_logger:info_report([ {conn, Conn}, {direction, Direction}, {binary, Binary} ]).

