-module(event_server).
-export([start/0]).

-record(state, {clients, events}).

start() ->
    spawn(fun() -> loop(#state{clients=orddict:new(), events=orddict:new()}) end).

loop(State = #state{clients=Clients, events=Events}) ->
    receive
        { subscribe, Client } ->
            Ref = erlang:monitor(process, Client),
            Client ! { self(), ok },
            loop(State#state{clients=orddict:store(Ref, { Client }, Clients), events=Events});
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
            [ Client ! { done, Name, Description } || { _Key, { Client } } <- orddict:to_list(Clients) ],
            UpdatedEvents = orddict:erase(Name, Events),
            loop(State#state{clients=Clients, events=UpdatedEvents});
        {'DOWN', Ref, process, Pid, _Reason} ->
            case orddict:find(Ref, Clients) of
                { ok, { Pid }} ->
                    io:format("[Server] Remove Client here ~p.~n", [Ref]),
                    UpdatedClients = orddict:erase(Ref, Clients),
                    loop(State#state{clients=UpdatedClients, events=Events});
                error ->
                    loop(State#state{clients=Clients, events=Events})
                end;
        { _Ref, ok } ->
            io:format("[Server] Event successfully cancelled.~n"),
            loop(State);
        { shutdown } ->
            io:format("[Server] So long!~n"),
            exit(terminated);
        Message ->
            io:format("[Server] Unexpected item in the messaging area: ~p.~n", [Message]),
            loop(State)
    end.
