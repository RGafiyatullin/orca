-module (orca_decoder_handshake).
-compile ({parse_transform, gin}).
-export ([
		decode/1
	]).
-include("types.hrl").
-include("proto_consts.hrl").

-type prop() :: {atom(), binary() | integer()}.
-spec decode( binary() ) -> {ok, {handshake_request, [ prop() ]}} | {ok, handshake_error} | {error, len_of_auth_plugin_data_is_zero}.
decode( Bin ) -> decode_handshake_request( Bin ).


%%% Protocol::HandshakeV10 %%%

decode_handshake_request( Bin0 ) when is_binary( Bin0 ) ->
	{ok, ProtoVer, Bin1} = orca_type_decoder:take( {int, 1}, Bin0 ),
	case ProtoVer of
		16#FF -> {ok, handshake_error};
		16#0A -> % Protocol::HandshakeV10
			{ok, ServerVersion, Bin2} = orca_type_decoder:take( {string, null}, Bin1 ),
			{ok, ConnectionID, Bin3} = orca_type_decoder:take( {int, 4}, Bin2 ),
			{ok, AuthPluginDataPart1, Bin4} = orca_type_decoder:take( {string, 8}, Bin3 ),
			{ok, 16#00, Bin5} = orca_type_decoder:take( {int, 1}, Bin4 ),
			{ok, CapFlags_Lo, Bin6} = orca_type_decoder:take( {int, 2}, Bin5 ),
			Props0 = [
					{server_version, ServerVersion},
					{connection_id, ConnectionID},
					{auth_plugin_data_part_1, AuthPluginDataPart1},
					{cap_flags_lo, CapFlags_Lo},
					{cap_flags, CapFlags_Lo}
				],
			case Bin6 of
				<<>> -> {ok, {handshake_request, Props0}};
				MoreDataInPacket = << _:1/binary, _/binary >> ->
					decode_handshake_request_extended( MoreDataInPacket, Props0 )
			end
	end.

decode_handshake_request_extended( Bin0, Props0 ) ->
	{ok, Encoding, Bin1} = orca_type_decoder:take( {int, 1}, Bin0 ),
	{ok, StatusFlags, Bin2} = orca_type_decoder:take( {int, 2}, Bin1 ),
	{ok, CapFlags_Hi, Bin3} = orca_type_decoder:take( {int, 2}, Bin2 ),

	{value, {cap_flags, CapFlags_Lo}, Props1} = lists:keytake( cap_flags, 1, Props0 ),
	CapFlags = CapFlags_Lo + (CapFlags_Hi * 16#10000),

	Props2 = [
		{encoding, Encoding},
		{status_flags, StatusFlags},
		{cap_flags_hi, CapFlags_Hi},
		{cap_flags, CapFlags}
		| Props1 ],

	{ok, LenOfAuthPluginData, Bin4} = orca_type_decoder:take( {int, 1}, Bin3 ),
	{ok, _Reserved10Bytes, Bin5} = orca_type_decoder:take( {string, 10}, Bin4 ),
	case {
		cap_check( ?CAP_CLIENT_SECURE_CONNECTION, CapFlags ),
		cap_check( ?CAP_CLIENT_PLUGIN_AUTH, CapFlags ),
		LenOfAuthPluginData
	} of
		{true, true, 0} -> {error, len_of_auth_plugin_data_is_zero};
		{true, true, GtZ} when is_integer(GtZ) andalso GtZ > 0 ->
			decode_handshake_request_cap_secure_conn( LenOfAuthPluginData, Bin5, Props2 );
		{_, _, _} -> {ok, {handshake_request, Props2}}
	end.

decode_handshake_request_cap_secure_conn( LenOfAuthPluginData, Bin0, Props0 ) ->
	AuthPluginDataPart2Len = erlang:max( 13, LenOfAuthPluginData - 8 ),
	{ok, AuthPluginDataPart2, Bin1} = orca_type_decoder:take( {string, AuthPluginDataPart2Len}, Bin0 ),
	{ok, AuthPluginName, _MostProbablyEmptyBin} = orca_type_decoder:take( {string, null}, Bin1 ),
	{ok, {handshake_request, [
				{auth_plugin_data_part_2, AuthPluginDataPart2},
				{auth_plugin_name, AuthPluginName}
				| Props0
			]}}.

cap_check( Cap, Flags ) -> orca_caps:check( Cap, Flags ).



