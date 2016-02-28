-module(term_to_packet).
-export([term_to_packet/1, test/0]).


test() ->
    <<0,0,0,10,131,107,0,6,97,98,99,100,101,102>> = term_to_packet("abcdef"),
    tests_worked.

term_to_packet(Term) ->
    N = term_to_binary(Term),
    Size = byte_size(N),
    <<Size:32, N/binary>>.
