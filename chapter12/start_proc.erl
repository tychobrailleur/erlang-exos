-module(start_proc).
-export([start/2, tick/0, test/0]).


test() ->
    register(guardian, spawn(fun() -> guardian([]) end)),
    Instances = [ one, two ],
    [ start(test, fun tick/0) || _Instance <- Instances ],
    timer:sleep(20000),
    whereis(test) ! stop.

%% Example of use:
%%   start_proc:start(test, fun start_proc:tick/0).
%%
%% To stop:
%%   whereis(test) ! stop.
%%
%% We use a process to keep track of registered processes,
%% so that concurrent processes don't register processes with the sane name.
start(AnAtom, Func) ->
    Guardian = whereis(guardian),
    Guardian ! { self(), register, AnAtom },
    receive
        { _Server, ok } -> catch register(AnAtom, spawn(Func));
        _ -> io:format("  ~p already registered.~n", [AnAtom])
    end.


guardian(Registry) ->
    receive
        { Client, register, Atom } ->
            case lists:member(Atom, Registry) of
                true -> Client ! { self(), nok }, guardian(Registry);
                false -> Client ! { self(), ok }, guardian([Atom|Registry])
            end
    end.


tick() ->
    receive
        stop -> io:format("Stopping...~n"), void
    after 5000 ->
            io:format("Tick ~p!~n", [ self() ]),
            tick()
    end.
