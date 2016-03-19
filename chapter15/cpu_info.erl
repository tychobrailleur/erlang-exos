-module(cpu_info).
-export([display/0]).

display() ->
    io:format(os:cmd("lscpu")).
