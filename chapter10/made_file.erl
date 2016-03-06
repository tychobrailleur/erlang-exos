-module(made_file).
-export([parse_file/1, test/0]).

test() ->
    FirstLink = hd(parse_file("index.html")),
    FirstLink = "<a href=\"http://www.github.com/erlang/otp\"><img src=\"/img/GitHub-Mark-32px.png\"/></a>",
    tests_worked.

%% I use this exercise to rewrite this example from the book
%% to figure out how it works.

parse_file(File) ->
    {ok, Content} = file:read_file(File),
    get_links(Content).

get_links(Bin) ->
    Content = binary_to_list(Bin),
    get_links(Content, []).

get_links("<a href=" ++ Rest, Acc) ->
    Link = extract_link(Rest),
    case Link of
        "" -> get_links(Rest, Acc);
        _ -> get_links(Rest, lists:reverse(["<a href=" ++ Link|Acc]))
    end;
get_links([_|Rest], Acc) ->
    get_links(Rest, Acc);
get_links([], Acc) ->
    Acc.


%% Patterns like `Text ++ </a>` are illegal.
%% Using regexp here instead of reversing etc. like in the book.

extract_link(Text) ->
    case re:run(Text, "^(.+?)</a>", [{capture, first, list}]) of
        { match, [Link] } -> Link;
        nomatch -> ""
    end.
