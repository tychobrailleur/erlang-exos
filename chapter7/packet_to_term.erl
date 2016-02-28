-module(packet_to_term).
-export([packet_to_term/1, test/0]).

test() ->
    "abcdef" = packet_to_term(<<0,0,0,10,131,107,0,6,97,98,99,100,101,102>>),
    tests_worked.

packet_to_term(Packet) ->
    case Packet of
        <<N:32, Term/binary>> when byte_size(Term) =:= N ->
            binary_to_term(Term)
    end.
