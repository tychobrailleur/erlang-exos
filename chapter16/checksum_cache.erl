-module(checksum_cache).
-export([start/0, rpc/2]).

start() ->
    spawn(fun() -> checksum(orddict:new()) end).

rpc(Pid, File) ->
    Pid ! { self(), {md5, File}},
    receive
        {File, Sum, CacheHit} ->
            io:format("~p	~p (Cache? ~p)~n", [Sum, File, CacheHit])
    end.

checksum(Cache) ->
    receive
        { Client, { md5, File }} ->
            case orddict:find(File, Cache) of
                {ok, {Sum, LastModified}} ->
                    Current = filelib:last_modified(File),
                    case is_date_before(LastModified, Current) of
                        true ->
                            io:format("Out of date, recomputing md5.~n"),
                            NewSum = md5_file:large_file_md5(File),
                            NewCache = orddict:store(File, {NewSum, Current}, Cache),
                            Client ! { File, NewSum, true },
                            checksum(NewCache);
                        false ->
                            Client ! { File, Sum, true },
                            checksum(Cache)
                    end;
                error ->
                    Sum = md5_file:large_file_md5(File),
                    NewCache = orddict:store(File, {Sum, filelib:last_modified(File)}, Cache),
                    Client ! { File, Sum, false },
                    checksum(NewCache)
            end;
        {_Client, list } ->
            io:format("Current state of cache: ~p.~n", [Cache]),
            checksum(Cache);
        _ -> checksum(Cache)
    end.

is_date_before(Date1, Date2) ->
    {DateDiff, {H,M,S}} = calendar:time_difference(Date2, Date1),
    DateDiff < 0 orelse H < 0 orelse M < 0 orelse S < 0.
