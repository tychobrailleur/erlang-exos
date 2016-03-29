-module(count).
-export([me/2, current_value/2, list_values/1, start/0]).

create_if_missing(Table) ->
    case ets:info(Table) of
        undefined -> ets:new(Table, [public, named_table]);
        _Info -> Table
    end.

me(Mod, Line) ->
    Table = create_if_missing(Mod),
    LineCount = ets:lookup(Table, Line),
    increment_count(Table, Line, LineCount).

increment_count(Table, Line, []) ->
    ets:insert(Table, {Line, 1}),
    Table;
increment_count(Table, Line, [{_L, Count}]) ->
    ets:insert(Table, {Line, Count+1}),
    Table.

current_value(Mod, Line) ->
    Val = ets:lookup(Mod, Line),
    case Val of
        [] -> 0;
        [{_L, Value}] -> Value
    end.

list_values(Table) ->
    ets:tab2list(Table).

start() ->
    count:me(?MODULE, ?LINE),
    io:format("Called count~n"),
    count:me(?MODULE, ?LINE),
    io:format("Called count again.~n"),
    list_values(count).
