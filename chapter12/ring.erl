-module(ring).
-export([test/0]).


test() ->
    N = 1000,
    M = 5,
    Processes = lists:seq(0, N-1),
    [ register(node_name(Process), spawn(fun() -> ring_node(Process, 1, M, N) end)) || Process <- Processes ],
    proc1 ! hello,
    done.

node_name(Index) ->
    list_to_atom("proc" ++ integer_to_list(Index)).

next_node(Index, N) ->
    NextIndex = ((Index + 1) rem N),
    node_name(NextIndex).

ring_node(Process, Count, M, N) ->
    receive
        hello ->
            { registered_name, Info } = process_info(self(), registered_name),
            io:format(" Message received by ~p: (~p) ~p.~n", [ self(), Count, Info ]),

            case whereis(next_node(Process, N)) of
                undefined -> nothing;
                Pid -> Pid ! hello
            end,
            case Count =:= M of
                false -> ring_node(Process, Count+1, M, N);
                _ -> void
            end
    end.
