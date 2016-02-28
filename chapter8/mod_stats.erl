-module(mod_stats).
-export([gen_stats/0]).

mod_functions({Mod, _File}) ->
    Stats = apply(Mod, module_info, []),
    [{exports, Functions}] = lists:filter(fun({Prop, _}) -> Prop =:= exports end, Stats),
    {Mod, Functions}.

gen_stats() ->
    Mods = lists:map(fun(N) -> mod_functions(N) end, code:all_loaded()),
    ModsCount = lists:sort(fun({_ModA, CountA}, {_ModB, CountB}) -> CountB < CountA end,
                           lists:map(fun({Mod, Func}) -> { Mod, length(Func) } end, Mods)),
    io:format(" Mod that exports the most functions: ~p.~n", [lists:nth(1, ModsCount)]),
    io:format("Mods: ~p.~n", [ModsCount]).
