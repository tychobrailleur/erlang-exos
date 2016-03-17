-module(monitor_killer).
-export([test/0, monitor_killer_restarter/1, worker/1, spawn_and_next/1]).


worker(Name) ->
    timer:sleep(2000),
    io:format("~p still working.~n", [Name]),
    worker(Name).

spawn_and_next([Worker|[]]) ->
    io:format("Last = ~p.~n", [Worker]),
    register(Worker, self()),
    worker(Worker);
spawn_and_next([Worker|Rest]) ->
    io:format("Next = ~p.~n", [Worker]),
    spawn_link(fun() -> spawn_and_next(Rest) end),
    register(Worker, self()),
    worker(Worker).

monitor_killer_restarter(Workers) ->
    spawn_monitor(?MODULE, spawn_and_next, [Workers]),
    receive
        { 'DOWN', _Ref, process, Pid, _Why } ->
            io:format("Process killed ~p.~n", [Pid]),
            monitor_killer_restarter(Workers)
    end.

start_monitor_restarter() ->
    spawn(fun() ->
                  Workers = [ list_to_atom("worker" ++ integer_to_list(I)) || I <- lists:seq(1, 10) ],
                  spawn(?MODULE, monitor_killer_restarter, [Workers])
          end).

test() ->
    start_monitor_restarter(),
    done.

%% You can kill any worker by running:
%%
%% `exit(whereis(worker8), kill)`
