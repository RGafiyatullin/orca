-ifndef(orca_include_orca_hrl).
-define(orca_include_orca_hrl, true).

-record(orca_ok, {
		affected_rows = 0 :: non_neg_integer(),
		last_inserted_id = 0 :: integer(),
		status_flags = 0 :: non_neg_integer(),
		number_of_warnings = 0 :: non_neg_integer(),
		msg = <<>> :: binary()
	}).
-record(orca_rows, {
		rows = [] :: [ [term()] ],
		field_defs = [] :: [ [term()] ],
		status_flags :: non_neg_integer(),
		number_of_warnings :: non_neg_integer()
	}).
-record(orca_error, {
		code = 0 :: non_neg_integer(),
		msg = <<>> :: binary(),
		sql_state_marker = <<>> :: binary(),
		sql_state = <<>> :: binary()
	}).
-record(orca_request_local_file_content, {
		filename :: binary()
	}).

-endif. % orca_include_orca_hrl
