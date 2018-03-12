-ifdef(use_os_system_time).
-define(now_ms, os:system_time(millisecond)).
-else.
-define(now_ms,
    case erlang:now() of
        {MegS, S, MuS} ->
            (((MegS * 1000000) + S) * 1000) + (MuS div 1000)
    end
).
-endif.
