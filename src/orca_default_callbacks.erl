-module (orca_default_callbacks).
-compile ({parse_transform, gin}).
-export ([log_error_logger/2, log_null/2]).


log_error_logger( LogLevel, Report ) ->
	F =
		case LogLevel of
			info -> info_report;
			warning -> warning_report;
			error -> error_report
		end,
	ok = error_logger:F( Report ).

log_null( _LogLevel, _Report ) -> ok.

