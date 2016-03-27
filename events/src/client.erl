-module(client).
-export([test/0, start/0, register/1, cancel/2, add_event/4, loop/0]).

test() ->
    Pid = event_server:start(),
    register(Pid),
    add_event(Pid, "Test", "Awesome test", 5000),
    add_event(Pid, "Test2", "Awesome test", 500000),
    Pid.


start() ->
    spawn(fun() -> loop() end).

register(Server) ->
    erlang:monitor(process, Server),
    Server ! { subscribe, self() }.

add_event(Server, Name, Description, Timeout) ->
    Server ! { self(), { add , Name, Description, Timeout } }.

cancel(Server, Name) ->
    Server ! { cancel, Name }.

loop() ->
    receive
        { done, Name, Description } ->
            io:format("[Client] Event ~p occurred: ~p.~n", [ Name, Description ]),
            loop();
        Message ->
            io:format("[Client] Got message = ~p.~n", [Message]),
            loop()
    end.
