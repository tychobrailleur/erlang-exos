-module(ets_test).
-export([start/0]).

start() ->
    lists:foreach(fun test_ets/1, [set, ordered_set, bag, duplicate_bag]).

test_ets(Mode) ->
    TableIds = ets:new(tet, [Mode]),
    ets:insert(TableIds, {a,1}),
    ets:insert(TableIds, {b,2}),
    ets:insert(TableIds, {a,1}),
    ets:insert(TableIds, {a,3}),
    List = ets:tab2list(TableIds),
    io:format("~-13w  => ~p~n", [Mode, List]),
    ets:delete(TableIds).
