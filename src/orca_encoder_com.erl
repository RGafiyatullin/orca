-module (orca_encoder_com).
-compile ({parse_transform, gin}).
-export ([com_query/2, com_query/1]).
-export ([com_init_db/1]).
-export ([com_process_info/0]).
-export ([com_ping/0]).
-include ("proto_consts.hrl").

com_query( Q ) -> com_query( Q, [] ).
com_query( Q, A ) -> orca_encoder_com_query:encode( Q, A ).

com_init_db( DbName ) when is_binary( DbName ) -> {ok, <<?COM_INIT_DB:8/integer, DbName/binary>>}.
com_process_info() -> {ok, <<?COM_PROCESS_INFO>>}.
com_ping() -> {ok, <<?COM_PING>>}.
