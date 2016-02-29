-module(mod_stats).
-export([gen_stats/0]).

mod_functions({Mod, _File}) ->
    Stats = apply(Mod, module_info, []),
    Functions = hd([Details || { exports, Details } <- Stats]),
    {Mod, Functions}.

gen_stats() ->
    Mods = lists:map(fun(N) -> mod_functions(N) end, code:all_loaded()),

    %% Sort modules by decreasing number of exported functions.
    ModsCount = lists:sort(fun({_ModA, CountA}, {_ModB, CountB}) -> CountB < CountA end,
                           lists:map(fun({Mod, Func}) -> { Mod, length(Func) } end, Mods)),

    io:format(" Mod that exports the most functions: ~p.~n", [hd(ModsCount)]),
    io:format(" Mods: ~p.~n", [ModsCount]).
