-module(erlang_tips).
-compile([export_all]).
-include_lib("stdlib/include/qlc.hrl").

-define(SALT_LENGTH, 12).

-record(user, { email, name, password, salt }).
-record(tip, { url, description, review_date }).
-record(abuse, { ip, visit_number }).


init() ->
    random:seed(erlang:time_offset(),
                erlang:monotonic_time(),
                erlang:unique_integer()).

setup_schema() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(users, [{ram_copies, [node()]},
                                {record_name, user},
                                {attributes, record_info(fields, user)}]),
    mnesia:create_table(tips, [{ram_copies, [node()]},
                               {record_name, tip},
                               {attributes, record_info(fields, tip)}]),
    mnesia:create_table(abuse, [{ram_copies, [node()]},
                                {record_name, abuse},
                                {attributes, record_info(fields, abuse)}]),
    mnesia:stop().

random_string(Length) ->
    lists:map(fun(_) -> crypto:rand_uniform(48, 122) end, lists:seq(1, Length)).


add_user(Name, Email, Password) ->
    Salt = random_string(?SALT_LENGTH),
    Row = #user{email=Email, name=Name, password=crypto:hash(sha256, Password ++ Salt), salt=Salt},
    F = fun() ->
                mnesia:write(users, Row, write)
        end,
    mnesia:transaction(F).

get_user(Email) ->
    F = fun() -> mnesia:read(users, Email) end,
    mnesia:transaction(F).

list_users() ->
    do(qlc:q([ {U#user.email, U#user.name} || U <- mnesia:table(users) ])).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    { atomic, Val } = mnesia:transaction(F),
    Val.
