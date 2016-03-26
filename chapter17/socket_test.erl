-module(socket_test).
-export([nano_get_url/1, nano_get_url/0]).

nano_get_url() ->
    nano_get_url("http://www.google.com").

nano_get_url(Url) ->
    {ok, {_Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(Url),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET " ++ (Path ++ Query) ++ " HTTP/1.1\r\nHost: " ++ Host ++ "\r\nAccept: text/html\r\nConnection: close\r\n\r\n"),
    Data = receive_data(Socket, []),
    nano_get_content(Data).

receive_data(Socket, Current) ->
    receive
        {tcp, Socket, Bin} ->
            receive_data(Socket, [Bin|Current]);
        {tcp_closed, Socket} ->
            list_to_binary(lists:reverse(Current))
    end.

nano_get_content(<<"HTTP/1.1 301 Found\r\n", Rest/binary>>) ->
    nano_get_url(find_redirect(Rest));
nano_get_content(<<"HTTP/1.1 302 Found\r\n", Rest/binary>>) ->
    nano_get_url(find_redirect(Rest));
nano_get_content(Other) -> Other.


find_redirect(Data) ->
    Tokens = re:split(Data, "\r\n"),
    LocationHeader = lists:filter(fun(X) -> case re:run(X, "^Location: (.*)$") of
                                                {match, _Capture} -> true;
                                                _ -> false
                                            end
                                  end, Tokens),
    find_location_value(hd(LocationHeader)).

find_location_value(<<"Location:", Value/binary>>) ->
    string:strip(binary_to_list(Value)).
