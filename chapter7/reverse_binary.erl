-module(reverse_binary).
-export([test/0, reverse/1]).

test() ->
    <<199,138,169>> = reverse(<<169,138,199>>),
    tests_worked.

reverse(<<>>) -> <<>>;
reverse(<<H, Rest/binary>>) -> list_to_binary([reverse(Rest), H]).
