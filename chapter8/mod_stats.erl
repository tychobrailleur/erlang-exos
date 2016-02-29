-module(mod_stats).
-export([gen_stats/0]).

%% Returns the module and its functions.
mod_functions({Mod, _File}) ->
    Stats = apply(Mod, module_info, []),
    Functions = hd([Details || { exports, Details } <- Stats]),
    {Mod, Functions}.

%% Returns a map with function name as key, and the number of occurrences
%% of that name as value.
count_functions(Functions) ->
    count_functions(Functions, #{}).

count_functions([Func|Rest], Map) ->
    Count = maps:get(Func, Map, 0),
    count_functions(Rest,  Map#{ Func => Count+1 });
count_functions([], Map) ->
    Map.

list_unambiguous_functions(Frequencies) ->
    maps:fold(fun(K, V, Acc) ->
                      case V =:= 1 of
                          true -> Acc ++ [K];
                          false -> Acc
                      end
              end, [], Frequencies).

gen_stats() ->
    Mods = lists:map(fun(N) -> mod_functions(N) end, code:all_loaded()),

    %% Sort modules by decreasing number of exported functions.
    ModsCount = lists:sort(fun({_ModA, CountA}, {_ModB, CountB}) -> CountB < CountA end,
                           lists:map(fun({Mod, Func}) -> { Mod, length(Func) } end, Mods)),

    %% List all existing function names, and create map of occurrences.
    FunctionCount = count_functions([ FuncName || { _Mod, F } <- Mods,  { FuncName, _Arity } <- F ]),
    PopularName = maps:fold(fun(K, V, Acc) ->
                                    {_Name,Arity} = Acc,
                                    case V > Arity of
                                        true -> {K, V};
                                        false -> Acc
                                    end
                            end, {nil, 0}, FunctionCount),
    UnambiguousFunctions = list_unambiguous_functions(FunctionCount),

    %%  io:format(" FunctionCount: ~p.~n", [FunctionCount]),
    io:format(" Mod that exports the most functions: ~p.~n", [hd(ModsCount)]),
    io:format(" Unambiguous functions: ~p.~n", [UnambiguousFunctions]),
    io:format(" Most popular function name: ~p.~n", [PopularName]).
