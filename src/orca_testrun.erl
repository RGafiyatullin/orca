-module (orca_testrun).
-compile (export_all).

srv() ->
	{ok, Conn} = orca_conn_srv:start_link( "localhost", 3306, [ {active, false} ] ),
	{ok, HandshakeReqPacket} = recv_packet( Conn ),
	{ok, {handshake_request, HandshakeReqProps}} = orca_decoder_handshake:decode( HandshakeReqPacket ),
	{ok, HandshakeRespPacket} = orca_encoder_handshake_response:auth(
		<<"root">>, <<"root">>, <<"jd_router">>, [
			{client, orca},
			{pid, pid_to_list(Conn)}
		], HandshakeReqProps ),

	ok = orca_conn_srv:send_packet( Conn, 1, HandshakeRespPacket ),
	{ok, {AuthResultType, AuthResultProps}} = recv_and_decode_generic( Conn ),

	{ok, COMInitDb} = orca_encoder_com:com_init_db( <<"jd_router">> ),
	ok = orca_conn_srv:send_packet( Conn, 0, COMInitDb ),
	{ok, {InitDbType, InitDbProps}} = recv_and_decode_generic( Conn ),

	{ok, COMQuery0} = orca_encoder_com:com_query( <<"SELECT * FROM mysql.innodb_index_stats">>, [] ),
	ok = orca_conn_srv:send_packet( Conn, 0, COMQuery0 ),
	{ok, {QueryResult0Type, QueryResult0Props}} = recv_and_decode_generic( Conn ),

	{ok, ResultSetProps} = orca_decoder_result_set:decode( QueryResult0Props ),

	_UseThemAll = [
				{handshake, lists:sort(HandshakeReqProps)},
				{auth, AuthResultType, lists:sort(AuthResultProps)},
				{init_db, InitDbType, InitDbProps},
				{query_0, QueryResult0Type, QueryResult0Props}
			],

	{ok, Conn,
			ResultSetProps
		}.

recv_packet( Conn ) when is_pid( Conn ) ->
	ok = orca_conn_srv:set_active( Conn, once ),
	receive
		{orca_packet, Conn, PacketBin} -> {ok, PacketBin}
	after
		5000 -> {error, timeout}
	end.

recv_and_decode_generic( Conn ) -> recv_and_decode_generic( Conn, undefined ).

recv_and_decode_generic( Conn, Ctx0 ) when is_pid( Conn ) ->
	{ok, PacketBin} = recv_packet( Conn ),
	DecodeResult =
		case Ctx0 of
			undefined -> orca_decoder_generic_response:decode( PacketBin );
			_Defined -> orca_decoder_generic_response:decode_continue( PacketBin, Ctx0 )
		end,
	case DecodeResult of
		{ok, Complete} -> {ok, Complete};
		{incomplete, Ctx1} -> recv_and_decode_generic( Conn, Ctx1 )
	end.
