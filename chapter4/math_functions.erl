-module(math_functions).
-export([even/1, odd/1, test/0, filter/2]).

test() ->
    true = even(4),
    false = even(3),
    true = odd(3),
    false = odd(4),
    [1,3,5,7] = filter(fun odd/1, lists:seq(1, 8)),
    {[2,4,6],[1,3,5]} = split1(lists:seq(1,6)),
    {[2,4,6],[1,3,5]} = split2(lists:seq(1,6)),
    tests_worked.

even(Number) ->
    (Number rem 2) == 0.

odd(Number) ->
    not even(Number).

filter(F, L) ->
    filter(F, L, []).

filter(_, [], Acc) ->
    lists:reverse(Acc);
filter(F, [H|L], Acc) ->
    case F(H) of
        true -> filter(F, L, [H|Acc]);
        _ -> filter(F, L, Acc)
    end.

split1(L) ->
    { filter(fun even/1, L), filter(fun odd/1, L) }.

split2(L) ->
    split2(L, {[], []}).
split2([], {Even, Odd}) -> {lists:reverse(Even), lists:reverse(Odd)};
split2([H|L], {Even, Odd}) ->
    case even(H) of
        true -> split2(L, { [H|Even], Odd});
        false -> split2(L, { Even, [H|Odd] })
    end.
