-ifndef(orca_include_proto_consts_hrl).
-define(orca_include_proto_consts_hrl, true).

-define( CAP_CLIENT_LONG_PASSWORD,         16#00000001 ).
-define( CAP_CLIENT_FOUND_ROWS,            16#00000002 ).
-define( CAP_CLIENT_LONG_FLAG,             16#00000004 ).
-define( CAP_CLIENT_CONNECT_WITH_DB,       16#00000008 ).
-define( CAP_CLIENT_NO_SCHEMA,             16#00000010 ).
-define( CAP_CLIENT_COMPRESS,              16#00000020 ).
-define( CAP_CLIENT_ODBC,                  16#00000040 ).
-define( CAP_CLIENT_LOCAL_FILES,           16#00000080 ).
-define( CAP_CLIENT_IGNORE_SPACE,          16#00000100 ).
-define( CAP_CLIENT_PROTOCOL_41,           16#00000200 ).
-define( CAP_CLIENT_INTERACTIVE,           16#00000400 ).
-define( CAP_CLIENT_SSL,                   16#00000800 ).
-define( CAP_CLIENT_IGNORE_SIGPIPE,        16#00001000 ).
-define( CAP_CLIENT_TRANSACTIONS,          16#00002000 ).
-define( CAP_CLIENT_RESERVED,              16#00004000 ).
-define( CAP_CLIENT_SECURE_CONNECTION,     16#00008000 ).
-define( CAP_CLIENT_MULTI_STATEMENTS,      16#00010000 ).
-define( CAP_CLIENT_MULTI_RESULTS,         16#00020000 ).
-define( CAP_CLIENT_PS_MULTI_RESULTS,      16#00040000 ).
-define( CAP_CLIENT_PLUGIN_AUTH,           16#00080000 ).
-define( CAP_CLIENT_CONNECT_ATTRS,         16#00100000 ).
-define( CAP_CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA,
                                           16#00200000 ).
-define( CAP_CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS,
                                           16#00400000 ).
-define( CAP_CLIENT_SESSION_TRACK,         16#00800000 ).
-define( CAP_CLIENT_DEPRECATE_EOF,         16#01000000 ).

%% Packet types
-define( PACKET_TYPE_OK,  16#00 ).
-define( PACKET_TYPE_ERR, 16#FF ).
-define( PACKET_TYPE_EOF, 16#FE ).

%% Commands

-define( COM_SLEEP,            16#00 ).
-define( COM_QUIT,             16#01 ).
-define( COM_INIT_DB,          16#02 ).
-define( COM_QUERY,            16#03 ).
-define( COM_FIELD_LIST,       16#04 ).
-define( COM_CREATE_DB,        16#05 ).
-define( COM_DROP_DB,          16#06 ).
-define( COM_REFRESH,          16#07 ).
-define( COM_SHUTDOWN,         16#08 ).
-define( COM_STATISTICS,       16#09 ).
-define( COM_PROCESS_INFO,     16#0A ).
-define( COM_CONNECT,          16#0B ).
-define( COM_PROCESS_KILL,     16#0C ).
-define( COM_DEBUG,            16#0D ).
-define( COM_PING,             16#0E ).
-define( COM_TIME,             16#0F ).
-define( COM_DELAYED_INSERT,   16#10 ).
-define( COM_CHANGE_USER,      16#11 ).
-define( COM_RESET_CONNECTION, 16#1F ).
-define( COM_DAEMON,           16#1D ).


%% AUTH PLUGIN
-define( MYSQL_NATIVE_PASSWORD, <<"mysql_native_password">> ).
-define( MYSQL_OLD_PASSWORD, <<"mysql_old_password">> ).

%% Field types
-define( MYSQL_TYPE_DECIMAL,     16#00 ).
-define( MYSQL_TYPE_TINY,        16#01 ).
-define( MYSQL_TYPE_SHORT,       16#02 ).
-define( MYSQL_TYPE_LONG,        16#03 ).
-define( MYSQL_TYPE_FLOAT,       16#04 ).
-define( MYSQL_TYPE_DOUBLE,      16#05 ).
-define( MYSQL_TYPE_NULL,        16#06 ).
-define( MYSQL_TYPE_TIMESTAMP,   16#07 ).
-define( MYSQL_TYPE_LONGLONG,    16#08 ).
-define( MYSQL_TYPE_INT24,       16#09 ).
-define( MYSQL_TYPE_DATE,        16#0A ).
-define( MYSQL_TYPE_TIME,        16#0B ).
-define( MYSQL_TYPE_DATETIME,    16#0C ).
-define( MYSQL_TYPE_YEAR,        16#0D ).
-define( MYSQL_TYPE_NEWDATE,     16#0E ).
-define( MYSQL_TYPE_VARCHAR,     16#0F ).
-define( MYSQL_TYPE_BIT,         16#10 ).
-define( MYSQL_TYPE_TIMESTAMP2,  16#11 ).
-define( MYSQL_TYPE_DATETIME2,   16#12 ).
-define( MYSQL_TYPE_TIME2,       16#13 ).
-define( MYSQL_TYPE_NEWDECIMAL,  16#F6 ).
-define( MYSQL_TYPE_ENUM,        16#F7 ).
-define( MYSQL_TYPE_SET,         16#F8 ).
-define( MYSQL_TYPE_TINY_BLOB,   16#F9 ).
-define( MYSQL_TYPE_MEDIUM_BLOB, 16#FA ).
-define( MYSQL_TYPE_LONG_BLOB,   16#FB ).
-define( MYSQL_TYPE_BLOB,        16#FC ).
-define( MYSQL_TYPE_VAR_STRING,  16#FD ).
-define( MYSQL_TYPE_STRING,      16#FE ).
-define( MYSQL_TYPE_GEOMETRY,    16#FF ).
-endif. % orca_include_proto_consts_hrl
