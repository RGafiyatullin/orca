-module (orca_encoder_com).
-compile ({parse_transform, gin}).
-export ([com_query/2, com_query/1]).
-export ([com_quit/0]).
-export ([com_init_db/1]).
-export ([com_process_info/0]).
-export ([com_ping/0]).
% -export ([com_create_db/1]).
% -export ([com_drop_db/1]).
-export ([com_process_kill/1]).
-export ([com_reset_connection/0]).
-include ("proto_consts.hrl").

com_query( Q ) -> com_query( Q, [] ).
com_query( Q, A ) -> orca_encoder_com_query:encode( Q, A ).

com_quit() -> {ok, <<?COM_QUIT>>}.
com_init_db( DbName ) when is_binary( DbName ) -> {ok, <<?COM_INIT_DB:8/integer, DbName/binary>>}.
com_process_info() -> {ok, <<?COM_PROCESS_INFO:8/integer>>}.
com_ping() -> {ok, <<?COM_PING:8/integer>>}.
% com_create_db( DbName ) when is_binary(DbName) -> {ok, <<?COM_CREATE_DB:8/integer, DbName/binary>>}.
% com_drop_db( DbName ) when is_binary(DbName) -> {ok, <<?COM_DROP_DB:8/integer, DbName/binary>>}.
com_process_kill( ThreadID ) when is_integer( ThreadID ) andalso ThreadID >= 0 andalso ThreadID =< 16#FFFFFFFF ->
	{ok, <<?COM_PROCESS_KILL:8/integer, ThreadID:32/little>>}.
com_reset_connection() -> {ok, <<?COM_RESET_CONNECTION:8/integer>>}.