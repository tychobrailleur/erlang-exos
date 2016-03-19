-module(md5_file).
-export([local_md5/1, large_file_md5/1]).

local_md5(File) ->
    {ok, Content} = file:read_file(File),
    md5_to_string(erlang:md5(Content)).

large_file_md5(File) ->
    case file:open(File, [read, raw, binary]) of
        {ok, Device} ->
            Context = erlang:md5_init(),
            read_block(Device, Context)
    end.

read_block(Device, Context) ->
    case file:read(Device, 65536) of
        {ok, Block} ->
            NewContext = erlang:md5_update(Context, Block),
            read_block(Device, NewContext);
        eof ->
            file:close(Device),
            md5_to_string(erlang:md5_final(Context))
    end.

convert_to_hex([H|Rest]) ->
    [ integer_to_list(H, 16) | convert_to_hex(Rest) ];
convert_to_hex([]) -> [].

md5_to_string(Bin) ->
    lists:foldl(fun(E, Acc) -> Acc ++ E end, "", convert_to_hex(binary_to_list(Bin))).
