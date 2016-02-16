-module(my_time_func).
-export([test/0, weekday/3]).

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


%%% Implement some of the datetime python functions



days_before_year(Year) ->
    Y = Year - 1,
    Y*365 + Y div 4 - Y div 100 + Y div 400.

days_in_month(Year, Month) when Month == 2 ->
    case calendar:is_leap_year(Year) of
        true -> 29;
        false -> 28
    end;
days_in_month(_, Month) when Month /= 2 ->
    case lists:member(Month, [1, 3, 5, 7, 8, 10, 12]) of
        true -> 31;
        false -> 30
    end.

days_before_month(Month, Year) ->
    days_before_month(Month, Year, 0).

days_before_month(1, _, Days) ->
    Days;
days_before_month(Month, Year, Days) ->
    days_before_month(Month-1, Year, days_in_month(Month, Year) + Days).

-spec weekday(Day, Month, Year) -> weekday() when
      Day :: pos_integer(),
      Month :: month(),
      Year :: pos_integer().
-type month() :: 1..12.
-type weekday() :: 1..7.

weekday(Day, Month, Year) ->
    Weekday =  (days_before_year(Year) + days_before_month(Month, Year) + Day) rem 7,
    case Weekday of
        0 -> 7;
        _ -> Weekday
    end.
