-module(my_spawn).
-export([my_spawn/3, test/0, test2/0, exiting_process/1, my_spawn/4]).

test() ->
    spawn(?MODULE, my_spawn, [?MODULE, exiting_process, [2000]]),
    done.

test2() ->
    spawn(?MODULE, my_spawn, [?MODULE, exiting_process, [1000], 1.5]),
    spawn(?MODULE, my_spawn, [?MODULE, exiting_process, [2000], 1.5]),
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

%% spawns a process and kills it if it runs longer than Time s.
my_spawn(Mod, Func, Args, Time) ->
    Start = os:timestamp(),
    { Pid, Ref } = spawn_monitor(Mod, Func, Args),

    receive
        { 'DOWN', Ref, process, Pid, Why } ->
            Diff = timer:now_diff(os:timestamp(), Start),
            io:format("Process ~p died after ~p ms: ~p.~n", [Pid, Diff, Why])
    after
        trunc(Time*1000) ->
            Diff = timer:now_diff(os:timestamp(), Start),
            io:format("Process ~p timed out after ~p ms.~n", [Pid, Diff]),
            exit(Pid, kill)
    end.

%% Test function for my_spawn.
exiting_process(Time) ->
    io:format("Running ~p.~n", [self()]),
    receive
    after
        Time ->
            io:format("Goodbye cruel world!~n"),
            exit(sayonara)
    end.
