-module(rolling_hash).
-export([run/0]).

-define(BLOCK_SIZE, 40).
-define(HASH_BASE, 257).
-define(MOD, 1000000093).
-define(HIGH_MULTIPLIER, bin_to_int(crypto:mod_pow(?HASH_BASE,  ?BLOCK_SIZE-1, ?MOD))).

run() ->
    Content = read_content(),
    Plagiarism = " lights begin to twinkle from the rocks ",
    Hash = hash(Plagiarism),
    io:format("Plagiarism = ~p.~n", [Hash]),
    Hashes = ets:new(?MODULE, [named_table]),
    compute_hashes(Content, Hashes),
    io:format("Found? ~p.~n", [ets:lookup(Hashes, Hash)]),
    ets:delete(Hashes).

compute_hashes(Text, Table) ->
    rolling_hash(Text, <<>>, 1, 0, fun(H, Pos) -> ets:insert(Table, {H, Pos}) end).

-spec rolling_hash(Text, PreviousHash, Position, Char, Func) -> integer() | done when
      Text::binary(),
      PreviousHash::binary(),
      Position::integer(),
      Char::char(),
      Func::function().

%% Text is the current remaining part of the text to be analyzed,
%% PreviousHash the previous value of the computed Hash,
%% Position the current position in the file,
%% FirstTerm is the previous first character of the hashed text chunk to be removed from hash,
%% and F the function to be applied to the newly computed hash.
%%
%% If no previous hash, use the `hash` function on the chunk,
%% then hash "rolls" in subsequent calls.
rolling_hash(Text, <<>>, Position, _FirstTerm, F) ->
    Chunk = string:sub_string(binary_to_list(Text), 1, ?BLOCK_SIZE),
    CurrentHash = hash(Chunk),
    F(CurrentHash, Position),
    rolling_hash(list_to_binary(string:sub_string(binary_to_list(Text), 2)), CurrentHash, Position+1, hd(Chunk), F);

rolling_hash(<<First, Rest/binary>>, PreviousHash, Position, FirstTerm, F) ->
    case size(Rest) >= ?BLOCK_SIZE of
        true -> CurrentHash = mod((?HASH_BASE*(PreviousHash-FirstTerm*?HIGH_MULTIPLIER) + lists:nth(?BLOCK_SIZE-1, binary_to_list(Rest))), ?MOD),
                F(CurrentHash, Position),
                rolling_hash(Rest, CurrentHash, Position+1, First, F);
        false -> done
    end.

%% Read the content of the file named `tennyson.txt`, convert it to lowercase,
%% and remove non-alphanumeric/whitespace characters by replacing them with a space.
read_content() ->
    {ok, Data} = file:read_file("tennyson.txt"),
    re:replace(string:to_lower(binary_to_list(Data)), "[^a-z0-9 ]", " ", [global, {return, binary}]).


hash(Str) ->
    hash(list_to_binary(Str), string:len(Str), 0).

%% Hashing function:
%% \[
%%     h(S) = \sum_{i=1}^n b^{i-1} S[i] \,\mathrm{mod}\,m \qquad \mathrm{with} \qquad b = 257, m = 1000000093.
%% \]
%%
%% Cf. http://courses.csail.mit.edu/6.006/spring11/rec/rec06.pdf
%%
hash(<<>>, _Len, Acc) ->
    mod(Acc, ?MOD);
hash(<<First, Rest/binary>>, Len, Acc) ->
    hash(Rest, Len-1, Acc+(First*bin_to_int(crypto:mod_pow(?HASH_BASE, Len-1, ?MOD)))).


%% Cf. https://github.com/erlang/otp/blob/maint/lib/crypto/src/crypto.erl#L1635
bin_to_int(Bin) when is_binary(Bin) ->
    Bits = bit_size(Bin),
    <<Integer:Bits/integer>> = Bin,
    Integer.

mod(0, _) -> 0;
mod(A, B) when A > 0 -> A rem B;
mod(A, B) when A < 0 -> B + A rem B.
