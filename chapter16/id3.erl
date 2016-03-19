-module(id3).
-export([list_mp3s/1, list_mp3_id3s/1]).

-record(metadata, {title, artist, album, year, comment, genre, track}).

list_mp3s(Dir) ->
    filelib:wildcard(Dir ++ '/**/*.mp3').

list_mp3_id3s(Dir) ->
    [ read_file(File) || File <- list_mp3s(Dir) ].

read_file(File) ->
    case file:open(File, [read, binary, raw]) of
        {ok, Device} ->
            % ID3v1 is located 128 bytes from the end.
            Size = filelib:file_size(File),
            {ok, Data} = file:pread(Device, Size-128, 128),
            Result = read_id3v1(Data),
            file:close(Device),
            Result;
        _Error -> error
    end.

read_id3v1(Data) ->
    parse_id3v1(Data).

%% id3 v1.1
parse_id3v1(<<$T, $A, $G, Title:30/binary, Artist:30/binary, Album:30/binary, Year:4/binary, Comment:28/binary, 0:8, Track:1/binary, Genre:1/binary>>) ->
    #metadata{title=remove_zeroes(binary_to_list(Title)),
              artist=remove_zeroes(binary_to_list(Artist)),
              album=remove_zeroes(binary_to_list(Album)),
              year=remove_zeroes(binary_to_list(Year)),
              comment=remove_zeroes(binary_to_list(Comment)),
              track=remove_zeroes(binary_to_list(Track)),
              genre=remove_zeroes(binary_to_list(Genre))};
%% id3 v1
parse_id3v1(<<$T, $A, $G, Title:30/binary, Artist:30/binary, Album:30/binary, Year:4/binary, Comment:30/binary, Genre:1/binary>>) ->
    #metadata{title=remove_zeroes(binary_to_list(Title)),
              artist=remove_zeroes(binary_to_list(Artist)),
              album=remove_zeroes(binary_to_list(Album)),
              year=remove_zeroes(binary_to_list(Year)),
              comment=remove_zeroes(binary_to_list(Comment)),
              genre=remove_zeroes(binary_to_list(Genre))};
parse_id3v1(_Other) -> error.


remove_zeroes(String) ->
    string:strip(lists:reverse(remove_leading_zeroes(lists:reverse(String)))).

remove_leading_zeroes([0|Rest]) ->
    remove_leading_zeroes(Rest);
remove_leading_zeroes(String) -> String.
