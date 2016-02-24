-module(myfile).
-export([read/1]).

read(File) ->
    case file:read_file(File) of
        {ok, Bin} -> Bin
        %% If not ok, case won't have any match and will throw an exception.
    end.
