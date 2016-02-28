-module(reverse_bit).
-export([reverse/1, test/0]).

test() ->
    <<0,1,0,1,0,1>> = reverse(<<1,0,1,0,1,0>>),
    tests_worked.


reverse(<<>>) -> <<>>;
reverse(<<H:1/binary, Rest/binary>>) ->
    Reverted =  reverse(Rest),
    << Reverted/binary, H/binary >>.
