-module(json_config).
-export([load_config/1, test/0]).


test() ->
    Config = load_config("./example.json"),
    <<"value">> = maps:get(<<"key">>, Config),
    tests_worked.

load_config(Path) ->
    {ok, Content} = read_file(Path),
    jsx:decode(Content, [return_maps]).

read_file(Path) ->
    file:read_file(Path).
