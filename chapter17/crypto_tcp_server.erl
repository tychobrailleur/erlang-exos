-module(crypto_tcp_server).
-export([start_nano_server/0, nano_client_eval/3]).

%% Use AES with ECB mode.
%% Cf. https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Electronic_Codebook_.28ECB.29

start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket).


loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n", [binary_to_blocks(Bin, 128)]),
            %% Use md5 to get a 128-bit key.
            Key = erlang:md5("password"),
            Data = [ strip_zeros(crypto:block_decrypt(aes_ecb, Key, Block)) || Block <- binary_to_blocks(Bin, 128) ],
            io:format("Decrypted data = ~p.~n", [Data]),
            {Mod, Func, Args} = binary_to_term(binary_join(Data)),
            io:format("Server (unpacked)  ~p:~p(~p)~n", [Mod, Func, Args]),
            Reply = apply(Mod, Func, Args),
            io:format("Server replying = ~p~n", [Reply]),
            gen_tcp:send(Socket, term_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.



nano_client_eval(Mod, Func, Args) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345, [binary, {packet, 4}]),
    Key = erlang:md5("password"),
    PlainText = term_to_binary({Mod, Func, Args}),
    io:format("Original text = ~p~n", [PlainText]),
    Data = binary_to_blocks(PlainText, 128),
    CipheredBlocks = [ crypto:block_encrypt(aes_ecb, Key, Block) || Block <- Data ],
    io:format("Encrypted data = ~p~n", [CipheredBlocks]),
    ok = gen_tcp:send(Socket, binary_join(CipheredBlocks)),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            Val = binary_to_term(Bin),
            io:format("Client result = ~p~n", [Val]),
            gen_tcp:close(Socket)
    after 10000 ->
            io:format("Timeout.~n")
    end.

%% Strip trailing zero from a binary.
strip_zeros(Bin) ->
    strip_zeros0(reverse(Bin)).

strip_zeros0(<<0, Rest/binary>>) ->
    strip_zeros0(Rest);
strip_zeros0(Bin) ->
    reverse(Bin).

%% Reverse a binary.
reverse(Bin) ->
    reverse0(Bin, <<>>).

reverse0(<<>>, Acc) ->
    Acc;
reverse0(<<H:1/binary, Rest/binary>>, Acc) ->
    reverse0(Rest, <<H/binary, Acc/binary>>).


%% Lifted and adapted from cowboy:
%% https://github.com/spawngrid/cowboy_session/blob/master/src/cowboy_session_secure.erl#L63
pad(Width, Binary) ->
    case ((Width - size(Binary)*8 rem Width) rem Width) of
        0 -> Binary;
        N -> <<Binary/binary, 0:N>>
    end.

%% Breaks a binary down into a list of 16-byte blocks (despite BlockSize)
binary_to_blocks(Bin, BlockSize) ->
    read_block(BlockSize, Bin, []).

read_block(BlockSize, <<Block:16/binary, Rest/binary>>, Acc) when size(Rest) > 0 ->
    read_block(BlockSize, Rest, [Block|Acc]);
read_block(BlockSize, Other, Acc) ->
    lists:reverse([pad(BlockSize, Other)|Acc]).


%% Cf. https://coderwall.com/p/nmajna/joining-a-list-of-binaries-in-erlang
binary_join([]) ->
    <<>>;
binary_join([Part]) ->
    Part;
binary_join(List) ->
    lists:foldr(fun (A, B) ->
                        if
                            bit_size(B) > 0 -> <<A/binary, B/binary>>;
                            true -> A
                        end
                end, <<>>, List).
