-module (orca_encoder_handshake_response).
-compile ({parse_transform, gin}).
-export ([
		auth/6
	]).
-include("proto_consts.hrl").

-define(character_set, 8). %% UTF-8
-define(max_packet_size, 16#4000000). %% 64 MiBytes

auth( Username, Password, DatabaseName, ClientCapFlags, ConnAttrs, HandshakeProps )
	when is_binary( Username )
	andalso is_binary( Password )
	andalso is_binary( DatabaseName )
->
	[ Username ] = binary:split( Username, <<0>> ),

	MaxPacketSize = ?max_packet_size,
	CharacterSet = ?character_set,

	AuthResponse = auth_response( Password, HandshakeProps ),
	AuthResponseLen = size(AuthResponse),
	AuthResponseLen_LenEncInt = len_enc_int( AuthResponseLen ),

	AttrsRendered = render_attrs( ConnAttrs ),
	AttrsRenderedLen_LenEncInt = len_enc_int( size(AttrsRendered) ),

	{ok, <<
		ClientCapFlags:32/little,
		MaxPacketSize:32/little,
		CharacterSet:8/integer,
		0:(23 * 8)/little, %% Reserved
		Username/binary, 0:8/integer, %% Null-terminated string
		AuthResponseLen_LenEncInt/binary, AuthResponse/binary,
		% AuthResponseLen:8/integer, AuthResponse/binary,
		DatabaseName/binary, 0:8/integer,  %% Null-terminated string
		AttrsRenderedLen_LenEncInt/binary, AttrsRendered/binary
	>>}.


len_enc_int( I ) when I < 16#FB -> << I:8/integer >>;
len_enc_int( I ) when I =< 16#FFFF -> << 16#FC:8/integer, I:16/little >>;
len_enc_int( I ) when I =< 16#FFFFFF -> << 16#FD:8/integer, I:24/little >>;
len_enc_int( I ) when I =< 16#FFFFFFFFFFFFFFFF -> << 16#FE:8/integer, I:64/little >>.


auth_response( Password, HandshakeProps ) ->
	{auth_plugin_data_part_1, Challenge_P1} = lists:keyfind( auth_plugin_data_part_1, 1, HandshakeProps ),
	{auth_plugin_data_part_2, Challenge_P2} = lists:keyfind( auth_plugin_data_part_2, 1, HandshakeProps ),
	<< Salt:20/binary, _/binary >> = <<Challenge_P1/binary, Challenge_P2/binary>>,
	Stage1 = crypto:hash( sha, Password ),
	Stage2 = crypto:hash( sha, Stage1 ),
	Res = crypto:hash_final(
			crypto:hash_update(
				crypto:hash_update(
					crypto:hash_init( sha ),
					Salt
				), Stage2 ) ),
	bxor_binary( Res, Stage1 ).

render_attrs( ConnAttrs ) ->
	iolist_to_binary( lists:reverse( lists:foldl(
		fun( {K, V}, Acc ) ->
			KN = normalize_attr_key_or_value( K ),
			VN = normalize_attr_key_or_value( V ),
			KSize = size( KN ),
			VSize = size( VN ),
			KSizeLE = len_enc_int( KSize ),
			VSizeLE = len_enc_int( VSize ),
			[<< KSizeLE/binary, KN/binary, VSizeLE/binary, VN/binary >> | Acc]
		end,
		[], ConnAttrs) ) ).

normalize_attr_key_or_value( A ) when is_atom( A ) -> atom_to_binary( A, latin1 );
normalize_attr_key_or_value( L ) when is_list( L ) -> unicode:characters_to_binary( L );
normalize_attr_key_or_value( B ) when is_binary( B ) -> B.

bxor_binary( B1, B2 ) ->
	list_to_binary(
		lists:zipwith(
			fun erlang:'bxor'/2,
			binary_to_list( B1 ),
			binary_to_list( B2 )
		)
	).
