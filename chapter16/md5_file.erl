-module(md5_file).
-export([local_md5/1]).

local_md5(File) ->
    {ok, Content} = file:read_file(File),
    lists:foldl(fun(E, Acc) -> Acc ++ E end, "", convert_to_hex(binary_to_list(erlang:md5(Content)))).

convert_to_hex([H|Rest]) ->
    [ integer_to_list(H, 16) | convert_to_hex(Rest) ];
convert_to_hex([]) -> [].
