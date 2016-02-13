-module(my_time_func).
-export([test/0]).


test() ->
    my_time_func(timer, sleep, [1500]),
    my_date_string().


%% Uses deprecated erlang:now()...
%% my_time_func(Module, F, Args) ->
%%     {_, StartSecs, StartMicroSecs} = erlang:now(),
%%     apply(Module, F, Args),
%%     {_, EndSecs, EndMicroSecs} = erlang:now(),
%%     io:format("Execution took ~p μs.~n", [EndMicroSecs-StartMicroSecs+(EndSecs-StartSecs)*1000000]).

my_time_func(Module, F, Args) ->
    Start = erlang:system_time(micro_seconds),
    apply(Module, F, Args),
    End = erlang:system_time(micro_seconds),
    End-Start.

my_date_string() ->
    {Year, Month, Day} = erlang:date(),
    {Hour, Minute, Second} = erlang:time(),
    io:format("Today is ~p/~p/~p ~p:~p:~p.~n", [Day, Month, Year, Hour, Minute, Second]).
