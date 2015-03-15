-module (orca_test).
-compile ({parse_transform, gin}).
-compile (export_all).
-include ("orca.hrl").
-include ("proto_consts.hrl").

all() ->
	ok = lists:foreach(
		fun( F ) -> ok = F() end,
		[
			fun test_01_conn/0,
			fun test_02_conn/0,
			fun test_01_mgr/0,
			fun test_02_mgr/0
		] ).

db_url() -> <<"mysql://orca_test:orca_test_password@localhost/orca_test_db?pool_size=8&min_restart_interval=500">>.
db_opts() -> [
			{callback_log, fun orca_default_callbacks:log_null/2},
			{callback_log_tcp, fun orca_default_callbacks:log_tcp_null/3}
		].
verbose_tcp() -> {callback_log_tcp, fun orca_default_callbacks:log_tcp_error_logger/3}.
verbose_log() -> {callback_log, fun orca_default_callbacks:log_error_logger/2}.


sql_table_create_test_1() ->
	<<"CREATE TABLE test_1 ("
		"  id BIGINT AUTO_INCREMENT NOT NULL"
		", i INT NOT NULL"
		", vc VARCHAR(12) NOT NULL"
		", c CHAR(12) NOT NULL"
		", vb VARBINARY(12) NOT NULL"
		", b BINARY(12) NOT NULL"
		", dt DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP"
		", PRIMARY KEY (id)"
		", UNIQUE (i)"
		")">>.
sql_insert_into_test_1() ->
	<<"INSERT INTO test_1 ( i, vc, c, vb, b ) VALUES "
		"  (1, 'varchar1', 'char1', 'varbinary1', 'binary1')"
		", (2, 'varchar2', 'char2', 'varbinary2', 'binary2')"
		>>.
sql_select_from_test_1() ->
	<<"SELECT id, i, vc, c, vb, b, dt FROM test_1 ORDER BY i ASC">>.

test_01_conn() ->
	{ok, Conn} = orca_conn:start_link( db_url(), db_opts() ),
	test_01_gen( orca_conn, Conn ).

test_01_mgr() ->
	{ok, Mgr} = orca:start_link( db_url(), db_opts() ),
	ok = orca:await_ready( Mgr ),
	test_01_gen( orca, Mgr ).

test_02_conn() ->
	{ok, Conn} = orca_conn:start_link( db_url(), db_opts() ),
	test_02_gen( orca_conn, Conn ).

test_02_mgr() ->
	{ok, Mgr} = orca:start_link( db_url(), db_opts() ),
	ok = orca:await_ready( Mgr ),
	test_02_gen( orca, Mgr ).

test_01_gen( Mod, Orca ) ->
	{ok, #orca_ok{}} = Mod:ping( Orca ),
	ok = Mod:shutdown( Orca, normal ).

test_02_gen( Mod, Orca ) ->
	{ok, #orca_ok{}} = Mod:sql( Orca, <<"DROP TABLE IF EXISTS test_1">> ),
	{ok, #orca_ok{}} = Mod:sql( Orca, sql_table_create_test_1() ),
	{ok, #orca_ok{}} = Mod:sql( Orca, sql_insert_into_test_1() ),
	{ok, #orca_rows{ rows = [
			[_,1,<<"varchar1">>, <<"char1">>, <<"varbinary1">>, <<"binary1", 0:(5 * 8)/integer>>, {datetime, {_, _}}],
			[_,2,<<"varchar2">>, <<"char2">>, <<"varbinary2">>, <<"binary2", 0:(5 * 8)/integer>>, {datetime, {_, _}}]
		] }} = Mod:sql( Orca, sql_select_from_test_1() ),
	{ok, #orca_ok{}} = Mod:sql( Orca, <<"DELETE FROM test_1">> ),
	{ok, #orca_ok{}} = Mod:sql( Orca, <<"DROP TABLE test_1">> ),
	ok = Mod:shutdown( Orca, normal ).


test_03_conn() ->
	{ok, Conn} = orca_conn:start_link( db_url(), [ {cap_add, ?CAP_CLIENT_LOCAL_FILES} | db_opts() ] ),
	{ok, #orca_ok{}} = orca_conn:sql( Conn, <<"DROP TABLE IF EXISTS test_1">> ),
	{ok, #orca_ok{}} = orca_conn:sql( Conn, sql_table_create_test_1() ),
	{ok, #orca_request_local_file_content{ filename = <<"test-file.tsv">> }} =
		orca_conn:sql( Conn, <<
			"LOAD DATA LOCAL INFILE 'test-file.tsv' INTO TABLE test_1 "
			"(i, vc, c, vb, b)">> ),
	ok = orca_conn:raw_packet( Conn, 2, <<
			"1\tvarchar1\tchar1\tvarbinary1\tbinary1\n"
			"2\tvarchar2\tchar2\tvarbinary2\tbinary2\n"
		>> ),
	ok = orca_conn:raw_packet( Conn, 3, <<
			"3\tvarchar3\tchar3\tvarbinary3\tbinary3\n"
			"2\tvarchar4\tchar4\tvarbinary4\tbinary4\n"
		>> ),
	ok = orca_conn:raw_packet( Conn, 4, <<>> ),
	{ok, #orca_ok{ affected_rows = 3 }} = orca_conn:recv_response( Conn ),
	ok = orca_conn:shutdown( Conn, normal ).
