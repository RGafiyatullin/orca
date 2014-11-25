-module (orca_test).
-compile ({parse_transform, gin}).
-compile (export_all).
-include ("orca.hrl").

db_url() -> <<"mysql://orca_test:orca_test_password@localhost/orca_test_db">>.
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
		")">>.
sql_insert_into_test_1() ->
	<<"INSERT INTO test_1 ( i, vc, c, vb, b ) VALUES "
		"  (1, 'varchar1', 'char1', 'varbinary1', 'binary1')"
		", (2, 'varchar2', 'char2', 'varbinary2', 'binary2')"
		>>.
sql_select_from_test_1() ->
	<<"SELECT id, i, vc, c, vb, b, dt FROM test_1 ORDER BY i ASC">>.

test_01() ->
	{ok, Conn} = orca_conn:start_link( db_url() ),
	{ok, #orca_ok{}} = orca_conn:ping( Conn ),
	ok = orca_conn:shutdown( Conn, normal ).

test_02() ->
	{ok, Conn} = orca_conn:start_link( db_url() ),
	{ok, #orca_ok{}} = orca_conn:sql( Conn, <<"DROP TABLE IF EXISTS test_1">> ),
	{ok, #orca_ok{}} = orca_conn:sql( Conn, sql_table_create_test_1() ),
	{ok, #orca_ok{}} = orca_conn:sql( Conn, sql_insert_into_test_1() ),
	{ok, #orca_rows{ rows = [
			[_,1,<<"varchar1">>, <<"char1">>, <<"varbinary1">>, <<"binary1", 0:(5 * 8)/integer>>, {datetime, {_, _}}],
			[_,2,<<"varchar2">>, <<"char2">>, <<"varbinary2">>, <<"binary2", 0:(5 * 8)/integer>>, {datetime, {_, _}}]
		] }} = orca_conn:sql( Conn, sql_select_from_test_1() ),
	{ok, #orca_ok{}} = orca_conn:sql( Conn, <<"DELETE FROM test_1">> ),
	{ok, #orca_ok{}} = orca_conn:sql( Conn, <<"DROP TABLE test_1">> ),
	ok = orca_conn:shutdown( Conn, normal ).


