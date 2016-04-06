-module(erlang_tips2).
-export([init/0, setup_schema/0, add_user/3, get_user/1, list_users/0, add_tip/3, get_tip/1,
         list_tips/0, get_abuse/1, add_abuse/2, list_abuse/0, delete_tables/0]).
-include_lib("stdlib/include/qlc.hrl").

-define(SALT_LENGTH, 12).

-record(user, { email, name, password, salt }).
-record(tip, { url, description, review_date, user }).
-record(abuse, { timestamp, ip, user }).


%% erl -sname pantoufle -setcookie abc
%% > net_kernel:connect('pyjama@pyjama.local').
%% > mnesia:start().
%%

%% erl -sname pyjama -setcookie abc
%% > mnesia:start().
%% > erlang_tips2:setup_schema().

init() ->
    random:seed(erlang:time_offset(),
                erlang:monotonic_time(),
                erlang:unique_integer()).

setup_schema() ->
    mnesia:create_schema([node(), 'pantoufle@pantoufle.home']),
    mnesia:create_table(users, [{ram_copies, [node(), 'pantoufle@pantoufle.home']},
                                {record_name, user},
                                {attributes, record_info(fields, user)}]),
    mnesia:create_table(tips, [{ram_copies, [node(), 'pantoufle@pantoufle.home']},
                               {record_name, tip},
                               {attributes, record_info(fields, tip)}]),
    mnesia:create_table(abuse, [{ram_copies, [node(), 'pantoufle@pantoufle.home']},
                                {record_name, abuse},
                                {attributes, record_info(fields, abuse)}]).

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
    { atomic, Val } = mnesia:transaction(F),
    Val.

list_users() ->
    do(qlc:q([ {U#user.email, U#user.name} || U <- mnesia:table(users) ])).

add_tip(Url, Description, User) ->
    Row = #tip{url=Url, description=Description, user=User, review_date=erlang:localtime()},
    F = fun() ->
                mnesia:write(tips, Row, write)
        end,
    mnesia:transaction(F).

get_tip(Url) ->
    F = fun() -> mnesia:read(tip, Url) end,
    { atomic, Val } = mnesia:transaction(F),
    Val.

list_tips() ->
    do(qlc:q([ T || T <- mnesia:table(tips) ])).

get_abuse(Ip) ->
    F = fun() -> mnesia:read(abuse, Ip) end,
    { atomic, Val } = mnesia:transaction(F),
    Val.


add_abuse(Ip, User) ->
    Row = #abuse{timestamp=erlang:localtime(), ip=Ip, user=User},
    F = fun() ->
                mnesia:write(Row)
        end,
    mnesia:transaction(F).


list_abuse() ->
    do(qlc:q([ A || A <- mnesia:table(abuse) ])).


delete_tables() ->
    lists:foreach(fun(Table) -> mnesia:delete_table(Table) end, [abuse, tips, users]).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    { atomic, Val } = mnesia:transaction(F),
    Val.
