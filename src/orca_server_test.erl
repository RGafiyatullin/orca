-module (orca_server_test).
-compile ({parse_transform, gin}).
-compile (export_all).
-include ("orca.hrl").
-include ("proto_consts.hrl").

test_01() ->
	{ok, LSock} = gen_tcp:listen(3307, [ {active, false}, binary ]),
	{ok, Sock} = gen_tcp:accept( LSock ),
	{ok, Conn} = orca_conn_srv:start_link([
		{socket, Sock},
		{callback_log_tcp, fun orca_default_callbacks:log_tcp_error_logger/3} ]),
	ok = gen_tcp:controlling_process( Sock, Conn ),

	{ok, HandshakeReqBin} = orca_encoder_handshake:encode({handshake_request, []}),
	ok = orca_conn_srv:send_packet( Conn, 0, HandshakeReqBin ),
	{ok, HandshakeRespBin} = orca_conn_srv:recv_packet( Conn ),
	{ok, Conn, HandshakeRespBin}.


