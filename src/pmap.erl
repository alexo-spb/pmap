%% Copyright 2024 Aleksei Osin
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(pmap).

%% API
-export([
    map/3
]).

%% private API
-export([
    format_error/2
]).

-spec map(Fun, List1, MaxWorkers) -> List2 when
    Fun :: fun((term()) -> term()),
    List1 :: [term()],
    MaxWorkers :: pos_integer(),
    List2 :: [term()].
map(Fun, List, MaxWorkers)
  when not is_function(Fun, 1);
       not is_list(List);
       not is_integer(MaxWorkers); MaxWorkers < 1 ->
    error(badarg, [Fun, List, MaxWorkers], [{error_info, #{}}]);
map(Fun, [_, _|_] = List1, MaxWorkers) ->
    Pid = spawn_leader(Fun, List1, MaxWorkers),
    receive
        {Pid, ok, List2} ->
            List2;
        {Pid, error, Reason, Info} ->
            error(Reason, none, [{error_info, Info}])
    end;
map(Fun, List, _MaxWorkers) ->
    lists:map(Fun, List).

-spec spawn_leader(Fun, List, MaxWorkers) -> Pid when
    Fun :: fun((term()) -> term()),
    List :: [term()],
    MaxWorkers :: pos_integer(),
    Pid :: pid().
spawn_leader(Fun, List, MaxWorkers) ->
    spawn_link(leader_fun(self(), Fun, List, MaxWorkers)).

leader_fun(CallerPid, Fun, List, MaxWorkers) ->
    fun() -> leader_loop(CallerPid, Fun, List, MaxWorkers) end.

leader_loop(CallerPid, Fun, List, MaxWorkers) ->
    process_flag(trap_exit, true),
    leader_loop(CallerPid, Fun, MaxWorkers, _NumWorkers = 0, List, _Idx = 1, _Acc = []).

leader_loop(CallerPid, Fun, MaxWorkers, NumWorkers, [Item | List], Idx, Acc)
  when NumWorkers < MaxWorkers ->
    WorkerPid = spawn_worker(Fun),
    WorkerPid ! {map, Idx, Item},
    leader_loop(CallerPid, Fun, MaxWorkers, NumWorkers + 1, List, Idx + 1, Acc);
leader_loop(CallerPid, Fun, MaxWorkers, NumWorkers, List, Idx, Acc1)
  when NumWorkers > 0 ->
    receive
        {done, WorkerPid} ->
            case List of
                [Value | Rest] ->
                    WorkerPid ! {map, Idx, Value},
                    leader_loop(CallerPid, Fun, MaxWorkers, NumWorkers, Rest, Idx + 1, Acc1);
                _ ->
                    WorkerPid ! done,
                    leader_loop(CallerPid, Fun, MaxWorkers, NumWorkers, List, Idx, Acc1)
            end;
        {result, Result} ->
            Acc2 = [Result | Acc1],
            leader_loop(CallerPid, Fun, MaxWorkers, NumWorkers - 1, List, Idx, Acc2);
        {exception, WorkerPid, Details} ->
            unlink(CallerPid),
            Info = error_info({worker_exception, WorkerPid, Details}),
            CallerPid ! {self(), error, worker_exception, Info},
            error(worker_exception, none, [{error_info, Info}]);
        {'EXIT', CallerPid, Reason} ->
            Info = error_info({caller_exit, CallerPid, Reason}),
            error(caller_exit, none, [{error_info, Info}]);
        {'EXIT', WorkerPid, Reason} ->
            unlink(CallerPid),
            Info = error_info({worker_exit, WorkerPid, Reason}),
            CallerPid ! {self(), error, worker_exit, Info},
            error(worker_exit, none, [{error_info, Info}])
    end;
leader_loop(CallerPid, _Fun, _MaxWorkers, _NumWorkers, _List, _Idx, Acc) ->
    unlink(CallerPid),
    List = [Item || {_, Item} <- ordsets:union(Acc)],
    CallerPid ! {self(), ok, List}.

spawn_worker(Fun) ->
    spawn_link(worker_fun(self(), Fun)).

worker_fun(LeaderPid, Fun) ->
    fun() -> worker_loop(LeaderPid, Fun, _Acc = ordsets:new()) end.

worker_loop(LeaderPid, Fun, Acc) ->
    receive
        {map, Idx, Value1} ->
            case try_map(Fun, Value1) of
                {ok, Value2} ->
                    LeaderPid ! {done, self()},
                    worker_loop(LeaderPid, Fun, [{Idx, Value2} | Acc]);
                {exception, Class, Reason, Trace} ->
                    unlink(LeaderPid),
                    LeaderPid ! {exception, self(), {Idx, Value1, Class, Reason, Trace}},
                    erlang:raise(Class, Reason, Trace)
            end;
        done ->
            unlink(LeaderPid),
            LeaderPid ! {result, lists:reverse(Acc)}
    end.

try_map(Fun, Value1) ->
    try {ok, Fun(Value1)}
    catch
        Class:Reason:Trace ->
            {exception, Class, Reason, Trace}
    end.

error_info({caller_exit, CallerPid, Reason}) ->
    #{
        cause => #{
            pid => CallerPid,
            reason => Reason
        }
    };
error_info({worker_exit, WorkerPid, Reason}) ->
    #{
        cause => #{
            pid => WorkerPid,
            reason => Reason
        }
    };
error_info({worker_exception, WorkerPid, {Idx, Value, Class, Reason, Trace}}) ->
    #{
        cause => #{
            pid => WorkerPid,
            idx => Idx,
            value => Value,
            class => Class,
            reason => Reason,
            trace => Trace
        }
    }.

format_error(Reason, [{_Module, Function, Args, Extra} | _]) ->
    Info = proplists:get_value(error_info, Extra, #{}),
    Cause = maps:get(cause, Info, none),
    explain(Reason, Function, Args, Cause);
format_error(_Reason, _Trace) ->
    #{}.

explain(badarg, _, [Fun, List, MaxWorkers], _Cause) ->
    Info1 = #{},
    Info2 = case is_function(Fun, 1) of
        true -> Info1;
        false -> Info1#{1 => <<"is not a function of arity 2">>}
    end,
    Info3 = case is_list(List) of
        true -> Info2;
        false -> Info2#{2 => <<"is not a list of elements">>}
    end,
    Info4 = case is_integer(MaxWorkers) of
        true when MaxWorkers > 0 -> Info3;
        false -> Info3#{3 => <<"is not an integer bigger than 0">>}
    end,
    Info4;
explain(caller_exit, _Function, _Args, Cause) ->
    Format = <<
        "the caller process has been terminated~n"
        "     caller process pid: ~p~n"
        "     exit reason: ~p"
    >>,
    #{pid := Pid, reason := Reason} = Cause,
    Args = [Pid, Reason],
    #{general => io_lib:format(Format, Args)};
explain(worker_exit, _Function, _Args, Cause) ->
    Format = <<
        "the worker process has been terminated~n"
        "     worker process pid: ~p~n"
        "     exit reason: ~p"
    >>,
    #{pid := Pid, reason := Reason} = Cause,
    Args = [Pid, Reason],
    #{general => io_lib:format(Format, Args)};
explain(worker_exception, _Function, _Args, Cause) ->
    Format = <<
        "the worker process has been terminated~n"
        "     worker process pid: ~p~n"
        "     worker ~s~n"
        "     element position: ~w~n"
        "     element value: ~p"
    >>,
    #{pid := Pid, idx := Idx, value := Value} = Cause,
    #{class := Class, reason := Reason, trace := Trace} = Cause,
    Exception = erl_error:format_exception(Class, Reason, Trace, #{column => 6}),
    Args = [Pid, Exception, Idx, Value],
    #{general => io_lib:format(Format, Args)};
explain(_Reason, _Function, _Args, _Cause) ->
    #{}.
