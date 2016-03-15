-module(monitor_restarter).
-export([test/0, monitor_restarter/1, worker/1]).


worker(Name) ->
    timer:sleep(2000),
    io:format("~p still working.~n", [ Name ]),
    worker(Name).

monitor_restarter(Workers) ->
    receive
        { _Client, { add, Name } } ->
            io:format("Adding worker ~p.~n", [Name]),
            { Pid, Ref } = spawn_monitor(?MODULE, worker, [Name]),
            register(list_to_atom(Name), Pid),
            monitor_restarter(Workers#{ Ref => Name });
        { 'DOWN', Ref, process, Pid, Why } ->
            {ok, Name} = maps:find(Ref, Workers),
            io:format("Process ~p (~p) died: ~p.~n", [ Pid, Ref, Why ]),
            { NewPid, NewRef } = spawn_monitor(?MODULE, worker, [Name]),
            register(list_to_atom(Name), NewPid),
            RemainingWorkers = maps:remove(Ref, Workers),
            monitor_restarter(RemainingWorkers#{ NewRef => Name })
    end.

start_monitor_restarter() ->
    spawn(fun() ->
                  Pid = spawn(?MODULE, monitor_restarter, [#{}]),
                  [ Pid ! { self(), { add, "worker" ++ integer_to_list(I) } } || I <- lists:seq(1, 10) ]
          end).

test() ->
    start_monitor_restarter(),
    done.

%% You can kill any worker by running:
%%
%% `exit(whereis(worker8), kill)`
