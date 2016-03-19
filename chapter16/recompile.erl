-module(recompile).
-export([check_recompile/1, check_recompile_file/1]).

check_recompile(Dir) ->
    ErlFiles = filelib:wildcard(Dir ++ '/**/*.erl'),
    [ check_recompile_file(File) || File <- ErlFiles ].

check_recompile_file(File) ->
    Filename = string:sub_string(File, 1, string:rchr(File, $.)-1) ++ ".beam",
    case filelib:is_file(Filename) of
        true ->
            BeamDate = filelib:last_modified(Filename),
            ErlDate = filelib:last_modified(File),
            {File, is_date_before(BeamDate, ErlDate)};
        false -> {File, true}
    end.

is_date_before(Date1, Date2) ->
    {DateDiff, {H,M,S}} = calendar:time_difference(Date2, Date1),
    DateDiff < 0 orelse H < 0 orelse M < 0 orelse S < 0.
