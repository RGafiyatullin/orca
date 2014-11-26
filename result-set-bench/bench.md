    1> lists:foreach( fun( N ) -> orca_test:test_04_conn(N) end, [10, 100, 1000, 10000, 100000] ).

    =INFO REPORT==== 26-Nov-2014::14:14:24 ===
        orca_test
        test_04_gen
        mod: orca_conn
        row_count: 10
        insert_time: 2945
        insert_rate: 3395.585738539898

    =INFO REPORT==== 26-Nov-2014::14:14:24 ===
        orca_test
        test_04_gen
        mod: orca_conn
        row_count: 10
        select_time: 3947
        select_rate: 2533.569799847986

    =INFO REPORT==== 26-Nov-2014::14:14:24 ===
        orca_test
        test_04_gen
        mod: orca_conn
        row_count: 100
        insert_time: 26482
        insert_rate: 3776.1498376255568

    =INFO REPORT==== 26-Nov-2014::14:14:24 ===
        orca_test
        test_04_gen
        mod: orca_conn
        row_count: 100
        select_time: 1670
        select_rate: 59880.239520958086

    =INFO REPORT==== 26-Nov-2014::14:14:25 ===
        orca_test
        test_04_gen
        mod: orca_conn
        row_count: 1000
        insert_time: 263279
        insert_rate: 3798.252044409163

    =INFO REPORT==== 26-Nov-2014::14:14:25 ===
        orca_test
        test_04_gen
        mod: orca_conn
        row_count: 1000
        select_time: 12824
        select_rate: 77978.78976918278

    =INFO REPORT==== 26-Nov-2014::14:14:27 ===
        orca_test
        test_04_gen
        mod: orca_conn
        row_count: 10000
        insert_time: 2636664
        insert_rate: 3792.6713453060383

    =INFO REPORT==== 26-Nov-2014::14:14:28 ===
        orca_test
        test_04_gen
        mod: orca_conn
        row_count: 10000
        select_time: 128302
        select_rate: 77941.10769902261

    =INFO REPORT==== 26-Nov-2014::14:14:54 ===
        orca_test
        test_04_gen
        mod: orca_conn
        row_count: 100000
        insert_time: 26251979
        insert_rate: 3809.2366293604

    =INFO REPORT==== 26-Nov-2014::14:14:55 ===
        orca_test
        test_04_gen
        mod: orca_conn
        row_count: 100000
        select_time: 1363463
        select_rate: 73342.65762987334
    ok
    2>