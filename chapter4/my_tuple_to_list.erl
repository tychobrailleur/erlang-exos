-module(my_tuple_to_list).
-export([my_tuple_to_list/1]).

my_tuple_to_list(T) ->
    my_tuple_to_list(T, []).

my_tuple_to_list(T, Acc) when tuple_size(T) == 0 -> lists:reverse(Acc);
my_tuple_to_list(T, Acc) when tuple_size(T) > 0 ->
    First = element(1, T),
    my_tuple_to_list(erlang:delete_element(1, T), [ First | Acc ]).
