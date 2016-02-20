-module(my_hash).
-export([delete_if/2, is_empty/1, invert/1, test/0]).

test() ->
    test_delete_if(),
    test_is_empty(),
    test_invert(),
    test_worked.

test_delete_if() ->
    Hash = delete_if(fun(_K,_V) -> _V rem 2 /= 0 end, #{ "one" => 1, "two" => 2 }),
    error = maps:find("one", Hash),
    test_worked.

delete_if(Pred, Map) ->
    maps:fold(fun (_K, _V, Acc) ->
                      case Pred(_K, _V) of
                          false -> Acc#{ _K => _V };
                          _ -> Acc
                      end
              end, #{}, Map).


test_is_empty() ->
    {ok, 2} = maps:find("two", #{ "one" => 1, "two" => 2 }),
    false = is_empty(#{ "one" => 1 }),
    true = is_empty(#{}),
    test_worked.


is_empty(_H) when map_size(_H) == 0 -> true;
is_empty(_H) -> false.

test_invert() ->
    { ok, 1 } = maps:find("one", invert(#{ 1 => "one" })),
    tests_worked.

invert(_H) -> maps:fold(fun (_K, _V, Acc) -> Acc#{ _V => _K }  end, #{}, _H).
