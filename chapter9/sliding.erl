-module(sliding).
-export([sliding/2, test/0]).

test() ->
    [[1,2],[2,3],[3,4]] = sliding([1, 2, 3, 4], 2),
    [["I", "am"], ["am", "legend"]] = sliding(["I", "am", "legend"], 2),
    tests_worked.

%% Returns a list holding a sliding window of the input list, and of size Size.
-spec sliding(list(), pos_integer()) -> list().
sliding(List, Size) ->
    sliding(List, Size, []).


sliding([], _Size, Acc) ->
    Acc;
sliding(List, Size, Acc) when length(List) >= Size  ->
    Sublist = lists:sublist(List, Size),
    sliding(tl(List), Size, Acc ++ [Sublist]);
sliding(List, Size, Acc) when length(List) < Size ->
    sliding([], Size, Acc).
