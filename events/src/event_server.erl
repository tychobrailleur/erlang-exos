-module(event_server).
-export([start/0]).

-record(state, {clients, events}).

start() ->
    spawn(fun() -> loop(#state{clients=[], events=orddict:new()}) end).

loop(State = #state{clients=Clients, events=Events}) ->
    receive
        { subscribe, Client } ->
            erlang:monitor(process, Client),
            Client ! { self(), ok },
            loop(State#state{clients=[Client|Clients], events=Events});
        { Client, { add, Name, Description, Timeout } } ->
            Pid = event:start(self(), Name, Timeout),
            Ref = erlang:monitor(process, Pid),
            Client ! { self(), ok },
            UpdatedEvents = orddict:store(Name, { Ref, Pid, Description }, Events),
            loop(State#state{clients=Clients, events=UpdatedEvents});
        { cancel, Name } ->
            io:format("[Server] Request to cancel ~p.~n", [Name]),
            {ok, { Ref, Pid, _Description }} = orddict:find(Name, Events),
            erlang:demonitor(Ref),
            Pid ! { self(), Ref, cancel },
            UpdatedEvents = orddict:erase(Name, Events),
            loop(State#state{clients=Clients, events=UpdatedEvents});
        { Pid, { done, Name } } ->
            io:format("[Server] Got word that ~p is done.~n", [ Name ]),
            {ok, { Ref, Pid, Description }} = orddict:find(Name, Events),
            erlang:demonitor(Ref),
            [ Client ! { done, Name, Description } || Client <- Clients ],
            UpdatedEvents = orddict:erase(Name, Events),
            loop(State#state{clients=Clients, events=UpdatedEvents});
        {'DOWN', Ref, process, _Pid, _Reason} ->
            io:format("[Server] Remove Client here ~p.~n", [Ref]),
            loop(State);
        { shutdown } ->
            io:format("[Server] So long!~n"),
            exit(terminated);
        Message ->
            io:format("[Server] Unexpected item in the messaging area: ~p.~n", [Message])
    end.
