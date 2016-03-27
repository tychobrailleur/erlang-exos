-module(event).
-export([start/3]).

-record(state, {server, name, to_go=0}).

start(Server, Name, Delay) ->
    io:format("[Event] Starting ~p (expiring in ~p).~n", [ Name, Delay ]),
    spawn(fun() -> loop(#state{server=Server, name=Name, to_go=Delay}) end).

loop(State = #state{server=Server, name=Name, to_go=Delay}) ->
    receive
        { Server, Ref, cancel } ->
            io:format("[Event] Cancelling.~n"),
            Server ! { Ref, ok };
        Message -> io:format("[Event] Unexpected message = ~p.~n", [Message]),
                   loop(State)
    after Delay ->
            io:format("[Event] Event ~p done.~n", [ Name ]),
            Server ! { self(), { done, Name } }
    end.
