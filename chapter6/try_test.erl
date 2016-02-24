-module(try_test).
-export([nice_catch/1, test/0]).

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> error(a).

test() ->
    [ nice_catch(I) || I <- lists:seq(1, 5) ].


nice_catch(N) ->
    try generate_exception(N) of
        Val -> {N, normal, Val}
    catch
        throw:_ -> {"An exception has been thrown for " ++  integer_to_list(N), erlang:get_stacktrace()};
        exit:_ -> {"Exit has been called for " ++ integer_to_list(N), erlang:get_stacktrace()};
        error:_ -> {"Error raised for " ++ integer_to_list(N), erlang:get_stacktrace()}
    end.
