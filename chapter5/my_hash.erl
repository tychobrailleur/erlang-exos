-module(my_hash).
-export([delete_if/2, test/0]).

test() ->
    Hash = delete_if(fun(_K,_V) -> _V rem 2 /= 0 end, #{ "one" => 1, "two" => 2 }),
    error = maps:find("one", Hash),
    {ok, 2} = maps:find("two", Hash),
    test_worked.


delete_if(Pred, Map) ->
    maps:fold(fun (_K, _V, Acc) ->
                      case Pred(_K, _V) of
                          false -> Acc#{ _K => _V };
                          _ -> Acc
                      end
              end, #{}, Map).
