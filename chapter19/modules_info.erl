-module(modules_info).
-compile([export_all]).

start() ->
    Table = ets:new(?MODULE, [set]),
    process_all_exported_functions(Table),
    { { Fun, Arity }, Val } = hd(ets:lookup(Table, {rstr, 2})),
    io:format("Func ~p/~p is in mod `~p`~n", [Fun, Arity, Val]),
    ets:delete(Table).

insert_function(Table, Func, Mod) ->
    ets:insert(Table, { Func, Mod }).

process_exported_methods(F, Table, Mod) ->
    lists:map(fun(Func) -> F(Table, Func, Mod) end, Mod:module_info(exports)).

process_all_exported_functions(Table) ->
    AllModules = [ ModName || { ModName, _ } <- code:all_loaded() ],
    lists:map(fun(Mod) -> process_exported_methods(fun insert_function/3, Table, Mod) end, AllModules).
