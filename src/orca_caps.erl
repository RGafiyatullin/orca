-module (orca_caps).
-compile ({parse_transform, gin}).
-export ([
		check/2,
		flags/1, flags/2
	]).

flags( Caps ) -> flags( Caps, 0 ).
flags( [], Acc ) -> Acc;
flags( [ Cap | Caps ], Acc ) -> flags( Caps, Cap bor Acc ).

check( Cap, Flags ) when is_integer(Cap) andalso is_integer( Flags ) ->
	(Cap band Flags) /= 0.
