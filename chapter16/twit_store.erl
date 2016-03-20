-module(twit_store).
-export([init/1, store/2, fetch/1]).


-define(TWIT_SIZE, 140).

init(N) ->
    case file:open("twit_store.dat", [write, raw, binary]) of
        {ok, Device} ->
            file:allocate(Device, 0, N*?TWIT_SIZE),
            file:close(Device)
    end.

store(N, Buf) ->
    case file:open("twit_store.dat", [write, read, raw, binary]) of
        {ok, Device} ->
            file:pwrite(Device, (N-1)*?TWIT_SIZE, Buf),
            file:close(Device)
    end.

fetch(N) ->
    case file:open("twit_store.dat", [read, raw, binary]) of
        {ok, Device} ->
            {ok, Content} = file:pread(Device, (N-1)*?TWIT_SIZE, ?TWIT_SIZE),
            file:close(Device),
            Content
    end.
