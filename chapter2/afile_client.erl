-module(afile_client).
-export([ls/1, get_file/2, send_file/3]).


ls(Server) ->
    Server ! { self(), list_dir },
    receive
        {Server, File_list } ->
            File_list
    end.

get_file(Server, File) ->
    Server ! { self(), { get_file, File }},
    receive
        {Server, Content} ->
            Content
    end.

put_file(Server, Filename, Content) ->
    Server ! { self(), {put_file, Filename, Content}},
    receive
        {Server, Response} ->
            Response
    end.
