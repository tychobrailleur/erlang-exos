-module(search_pictures).
-export([search_pictures/1]).

search_pictures(Dir) ->
    Images = filelib:wildcard(Dir ++ "/**/*.{jpeg,JPEG,jpg,JPG}"),
    list_checksum(Images, orddict:new(), orddict:new()).

list_checksum([Image|Rest], Sums, Dupes) ->
    {NewSums, NewDupes} = checksum(Image, Sums, Dupes),
    list_checksum(Rest, NewSums, NewDupes);
list_checksum([], _Sums, Dupes) -> Dupes.

%% Compute the checksum for a large file, and add it to the list
%% of checksums if not already present; add it to the list of dupes
%% if it is.
checksum(File, Sums, Dupes) ->
    Sum = md5_file:large_file_md5(File),
    case orddict:find(Sum, Sums) of
        {ok, F} ->
            {Sums, orddict:append(Sum, [File|F], Dupes)};
        error ->
            {orddict:append(Sum, [File], Sums), Dupes}
    end.
