-module(math_functions).
-export([even/1, odd/1, test/0]).

test() ->
    true = even(4),
    false = even(3),
    true = odd(3),
    false = odd(4),
    tests_worked.

even(Number) ->
    (Number rem 2) == 0.

odd(Number) ->
    not even(Number).
