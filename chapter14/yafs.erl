-module(yafs).
-export([start_yafs/3]).



start_yafs(MM, _ArgsC, _ArgsS) ->
    loop(MM).


loop(MM) ->
    Dir = ".",

    receive
        { chan_closed, MM } ->
            true;
        { chan, MM, {_Client, list_dir} } ->
            MM ! { send, { self(), file:list_dir(Dir) } };
        { chan, MM, {_Client, {get_file, File}}} ->
            Full = filename:join(Dir, File),
            MM ! { send, { self(), file:read_file(Full) } };
        { chan, MM, {_Client, {put_file, File, Content}}} ->
            Full = filename:join(Dir, File),
            MM ! { send, { self(), file:write_file(Full, Content) } };
        Other ->
            io:format("Unknown message: ~p.~n", [Other])
    end,
    loop(MM).
