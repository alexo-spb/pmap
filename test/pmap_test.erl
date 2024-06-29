-module(pmap_test).

-include_lib("eunit/include/eunit.hrl").

api_test_() ->
    [
        {
            "map an empty list -> ok",
            fun() ->
                Fun = fun(_) -> ?assert(unexpected) end,
                List2 = List1 = [],
                ?assertEqual(List2, pmap:map(Fun, List1, _MaxWorkers = 10))
            end
        },
        {
            "map a list of 1 element -> ok",
            fun() ->
                Fun = fun(0) -> 1 end,
                List1 = [0],
                List2 = [1],
                ?assertEqual(List2, pmap:map(Fun, List1, _MaxWorkers = 10))
            end
        },
        {
            "map a list of 10 elements, use 1 worker -> ok",
            fun() ->
                Len = 10,
                Fun = fun(N) -> Len - 1 - N end,
                List1 = lists:seq(0, Len - 1),
                List2 = lists:reverse(List1),
                ?assertEqual(List2, pmap:map(Fun, List1, _MaxWorkers = 1))
            end
        },
        {
            "map a list of 10 elements, use 5 workers -> ok",
            fun() ->
                Len = 10,
                Fun = fun(N) -> {self(), Len - 1 - N} end,
                List1 = lists:seq(0, Len - 1),
                {Pids, List2} = lists:unzip(pmap:map(Fun, List1, MaxWorkers = 5)),
                List3 = lists:reverse(List1),
                ?assertEqual(List3, List2),
                ?assertEqual(MaxWorkers, length(lists:usort(Pids)))
            end
        },
        {
            "map a list of 10 elements, use 10 workers -> ok",
            fun() ->
                Len = 10,
                Fun = fun(N) -> {self(), Len - 1 - N} end,
                List1 = lists:seq(0, Len - 1),
                {Pids, List2} = lists:unzip(pmap:map(Fun, List1, MaxWorkers = 10)),
                List3 = lists:reverse(List1),
                ?assertEqual(List3, List2),
                ?assertEqual(MaxWorkers, length(lists:usort(Pids)))
            end
        },
        {
            "bad 1st argument -> error",
            fun() ->
                Fun = not_function,
                List = lists:seq(0, 9),
                ?assertError(badarg, pmap:map(Fun, List, _MaxWorkers = 10))
            end
        },
        {
            "bad 2nd argument -> error",
            fun() ->
                Fun = fun(_) -> ok end,
                List = not_list,
                MaxWorkers = 10,
                ?assertError(badarg, pmap:map(Fun, List, MaxWorkers))
            end
        },
        {
            "bad 3rd argument -> error",
            fun() ->
                Fun = fun(_) -> ok end,
                List = lists:seq(0, 9),
                MaxWorkers = not_integer,
                ?assertError(badarg, pmap:map(Fun, List, MaxWorkers))
            end
        },
        {
            "map a list of 10 elements, use 10 workers, 1st worker exception -> error",
            fun() ->
                Len = 10,
                TestPid = self(),
                Fun = fun
                    (0) ->
                        TestPid ! {worker_1, self()},
                        timer:sleep(1000),
                        error(test_reason);
                    (1) ->
                        TestPid ! {worker_2, self()},
                        timer:sleep(5000);
                    (_) ->
                        timer:sleep(5000)
                end,
                List = lists:seq(0, Len - 1),
                spawn(
                    fun() ->
                        try pmap:map(Fun, List, _MaxWorkers = 10)
                        catch
                            Class:Reason ->
                                TestPid ! {exception, {Class, Reason}}
                        end
                    end
                ),
                WorkerPid1 = receive {worker_1, Pid1} -> Pid1 end,
                WorkerRef1 = monitor(process, WorkerPid1),
                WorkerPid2 = receive {worker_2, Pid2} -> Pid2 end,
                WorkerRef2 = monitor(process, WorkerPid2),
                receive
                    {'DOWN', WorkerRef1, process, WorkerPid1, Reason1} ->
                        ?assertMatch({test_reason, _Trace}, Reason1)
                end,
                receive
                    {'DOWN', WorkerRef2, process, WorkerPid2, Reason2} ->
                        ?assertMatch({worker_exception, _Trace}, Reason2)
                end,
                receive
                    {exception, Exception} ->
                        ?assertMatch({error, worker_exception}, Exception)
                end
            end
        },
        {
            "map a list of 10 elements, use 10 workers, 1st worker exit -> error",
            fun() ->
                Len = 10,
                TestPid = self(),
                Fun = fun
                    (0) ->
                        TestPid ! {worker_1, Pid = self()},
                        spawn(fun() -> timer:sleep(1000), exit(Pid, kill) end),
                        timer:sleep(5000);
                    (1) ->
                        TestPid ! {worker_2, self()},
                        timer:sleep(5000);
                    (_) ->
                        timer:sleep(5000)
                end,
                List = lists:seq(0, Len - 1),
                spawn(
                    fun() ->
                        try pmap:map(Fun, List, _MaxWorkers = 10)
                        catch
                            Class:Reason ->
                                TestPid ! {exception, {Class, Reason}}
                        end
                    end
                ),
                WorkerPid1 = receive {worker_1, Pid1} -> Pid1 end,
                WorkerRef1 = monitor(process, WorkerPid1),
                WorkerPid2 = receive {worker_2, Pid2} -> Pid2 end,
                WorkerRef2 = monitor(process, WorkerPid2),
                receive
                    {'DOWN', WorkerRef1, process, WorkerPid1, Reason1} ->
                        ?assertMatch(killed, Reason1)
                end,
                receive
                    {'DOWN', WorkerRef2, process, WorkerPid2, Reason2} ->
                        ?assertMatch({worker_exit, _Trace}, Reason2)
                end,
                receive
                    {exception, Exception} ->
                        ?assertMatch({error, worker_exit}, Exception)
                end
            end
        },
        {
            "map a list of 10 elements, use 10 workers, caller exit -> error",
            fun() ->
                Len = 10,
                TestPid = self(),
                Fun = fun
                    (0) ->
                        TestPid ! {worker, self()},
                        timer:sleep(5000);
                    (_) ->
                        timer:sleep(5000)
                end,
                List = lists:seq(0, Len - 1),
                CallerPid = spawn(
                    fun() ->
                        pmap:map(Fun, List, _MaxWorkers = 10)
                    end
                ),
                WorkerPid = receive {worker, Pid} -> Pid end,
                WorkerRef = monitor(process, WorkerPid),
                exit(CallerPid, kill),
                receive
                    {'DOWN', WorkerRef, process, WorkerPid, Reason} ->
                        ?assertMatch({caller_exit, _Trace}, Reason)
                end
            end
        }
    ].
