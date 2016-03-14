-module(my_spawn).
-export([my_spawn/3, test/0, exiting_process/0]).

test() ->
    spawn(?MODULE, my_spawn, [?MODULE, exiting_process, []]),
    done.

%% spawns a process, and prints a message saying why the process died
%% and how long before it did.
my_spawn(Mod, Func, Args) ->
    Start = os:timestamp(),
    { Pid, Ref } = spawn_monitor(Mod, Func, Args),

    receive
        { 'DOWN', Ref, process, Pid, Why } ->
            Diff = timer:now_diff(os:timestamp(), Start),
            io:format("Process ~p died after ~p ms: ~p.~n", [Pid, Diff, Why])
    end.

exiting_process() ->
    timer:sleep(2000),
    io:format("Goodbye cruel world!~n"),
    exit(sayonara).
