-module (orca_encoder_handshake).
-compile ({parse_transform, gin}).
-export ([
		encode/1
	]).
-include("types.hrl").
-include("proto_consts.hrl").

-record(req, {
		server_version = <<"Orca Teapot">>:: binary(),
		connection_id = 0 :: non_neg_integer(),
		auth_plugin_data_part_1 = <<0:(8 * 8)/little>> :: binary(),
		cap_flags = 0 :: non_neg_integer(),
		status_flags = 0 :: non_neg_integer(),
		encoding = 8 :: non_neg_integer(),
		auth_plugin_data_part_2 = <<0:(13 * 8)/little>> :: binary(),
		auth_plugin_name = ?MYSQL_NATIVE_PASSWORD :: binary()
	}).

encode( {handshake_request, Props} ) ->
	case props_to_req( Props, {ok, #req{}} ) of
		BadPropsErr = {error, _} -> BadPropsErr;
		{ok, Req = #req{}} ->
			CapFlags0 = Req#req.cap_flags,
			CapFlags1 = orca_caps:flags( [
				?CAP_CLIENT_PROTOCOL_41,
				?CAP_CLIENT_PLUGIN_AUTH,
				?CAP_CLIENT_SECURE_CONNECTION ], CapFlags0 ),
			<< CapFlagsLo:(2 * 8)/little, CapFlagsHi:(2 * 8)/little >>
				= << CapFlags1:(4 * 8)/little >>,
			HandshakeBin = <<
					16#0A:8/integer, % Protocol version
					(Req#req.server_version)/binary, 0:8/integer,
					(Req#req.connection_id):(4 * 8)/little,
					(Req#req.auth_plugin_data_part_1)/binary,
					0:8/integer, % Filler
					CapFlagsLo:(2 * 8)/little,
					(Req#req.encoding):8/integer,
					(Req#req.status_flags):(2 * 8)/little,
					CapFlagsHi:(2 * 8)/little,
					(erlang:byte_size( Req#req.auth_plugin_data_part_2 )):8/integer,
					0:(10 * 8)/little,
					(Req#req.auth_plugin_data_part_2)/binary,
					(Req#req.auth_plugin_name)/binary, 0:8/integer
				>>,
			{ok, HandshakeBin}
	end.


props_to_req( _, {error, Reason} ) -> {error, Reason};
props_to_req( [], {ok, Req} ) -> {ok, Req};
props_to_req( [ {PName, PValue} | Props ], {ok, Req} ) ->
	props_to_req( Props, prop_set( PName, PValue, Req ) ).

-define( guard_flags( Max ), ( is_integer( PValue ) andalso PValue >= 0 andalso PValue =< Max ) ).
-define( prop( PN, Guard ), PN when Guard -> {ok, Req #req{ PN = PValue }} ).
prop_set( PName, PValue, Req = #req{} ) ->
	case PName of
		?prop( server_version, is_binary( PValue ) );
		?prop( connection_id, is_integer( PValue ) andalso PValue >= 0 );
		?prop( auth_plugin_data_part_1, is_binary( PValue ) andalso erlang:byte_size( PValue ) == 8 );
		?prop( cap_flags, ?guard_flags( 16#FFFFFFFF ) );
		?prop( encoding, ?guard_flags( 16#FF ) );
		?prop( status_flags, ?guard_flags( 16#FFFF ) );
		?prop( auth_plugin_data_part_2, is_binary( PValue ) andalso erlang:byte_size( PValue ) == 13 );
		?prop( auth_plugin_name, is_binary( PValue ) andalso PValue == ?MYSQL_NATIVE_PASSWORD );
		_ when in( PName, [
				server_version, connection_id, auth_plugin_data_part_1,
				cap_flags, encoding, status_flags,
				auth_plugin_data_part_2, auth_plugin_name
			] ) ->
				{error, {bad_propery_value, PName, PValue}};
		_ -> {error, {unknown_property, PName}}
	end.