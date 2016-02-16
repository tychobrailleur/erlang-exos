-module(map_search_pred).
-export([map_pred_search/2, test/0]).


test() ->
    {ok, Even} = map_pred_search(#{ "one" => 1, "two" => 2 }, fun (_K, _V) -> _V rem 2 == 0  end),
    Even = {"two", 2},
    {error, not_found} = map_pred_search(#{ "one" => 1, "two" => 2 }, fun (_K, _V) -> _V == 3  end),
    test_worked.


map_pred_search(Map, Pred) ->
    Matches = lists:filtermap(fun ({_K, _V}) -> Pred(_K, _V) end, maps:to_list(Map)),
    match_return(Matches).

match_return([]) ->
    { error, not_found };
match_return([L|_]) ->
    { ok, L }.
