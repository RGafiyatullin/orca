-module (orca_caps).
-compile ({parse_transform, gin}).
-export ([
		check/2,
		flags/1, flags/2,

		default_client_caps/0,
		cap_flags_from_opts/1
	]).
-include("proto_consts.hrl").

flags( Caps ) -> flags( Caps, 0 ).
flags( [], Acc ) -> Acc;
flags( [ Cap | Caps ], Acc ) -> flags( Caps, Cap bor Acc ).

check( Cap, Flags ) when is_integer(Cap) andalso is_integer( Flags ) ->
	(Cap band Flags) /= 0.

default_client_caps() -> [
		?CAP_CLIENT_PROTOCOL_41,

		?CAP_CLIENT_LONG_FLAG,
		?CAP_CLIENT_TRANSACTIONS,
		?CAP_CLIENT_MULTI_STATEMENTS,
		?CAP_CLIENT_MULTI_RESULTS,
		?CAP_CLIENT_SECURE_CONNECTION,
		?CAP_CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA,
		?CAP_CLIENT_CONNECT_ATTRS

		% , ?CAP_CLIENT_CONNECT_WITH_DB
		% , ?CAP_CLIENT_NO_SCHEMA
	].

cap_flags_from_opts( Opts ) ->
	ClientCapabilities = sets:to_list( lists:foldl(
		fun
			( {cap_add, C}, CapabilitiesAcc ) -> sets:add_element( C, CapabilitiesAcc );
			( {cap_rm, C}, CapabilitiesAcc ) -> sets:del_element( C, CapabilitiesAcc );
			( _, CapabilitiesAcc ) -> CapabilitiesAcc
		end,
		sets:from_list( default_client_caps() ), Opts ) ),
	_ClientCapFlags = orca_caps:flags( ClientCapabilities ).

