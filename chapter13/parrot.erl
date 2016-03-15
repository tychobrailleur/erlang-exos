-module(parrot).
-export([parrot/0, parrot_creator/0, start/0]).


parrot() ->
    receive
    after
        5000 -> io:format("I'm still alive.~n"),
                parrot()
    end.


parrot_creator() ->
    { Pid, Ref } = spawn_monitor(?MODULE, parrot, []),
    register(pearl_jam, Pid),

    receive
        { 'DOWN', Ref, process, Pid, Why } ->
            io:format("Process ~p died: ~p.~n", [Pid, Why]),
            parrot_creator()
    end.

start() ->
    spawn(?MODULE, parrot_creator, []).


%% Try killing the peal_jam process with the following:
%%
%% `exit(whereis(pearl_jam), kill).`
