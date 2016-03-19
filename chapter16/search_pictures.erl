-module(search_pictures).
-export([search_pictures/1]).

search_pictures(Dir) ->
    Images = filelib:wildcard(Dir ++ "/**/*.{jpeg,JPEG,jpg,JPG}"),
    Dupes = orddict:new(),
    Sums = orddict:new(),
    list_checksum(Images, Sums, Dupes).

list_checksum([Image|Rest], Sums, Dupes) ->
    {NewSums, NewDupes} = checksum(Image, Sums, Dupes),
    list_checksum(Rest, NewSums, NewDupes);
list_checksum([], _Sums, Dupes) -> Dupes.


checksum(File, Sums, Dupes) ->
    Sum = md5_file:large_file_md5(File),
    case orddict:find(Sum, Sums) of
        {ok, F} ->
            {Sums, orddict:append(Sum, [File|F], Dupes)};
        error ->
            {orddict:append(Sum, [File], Sums), Dupes}
    end.
