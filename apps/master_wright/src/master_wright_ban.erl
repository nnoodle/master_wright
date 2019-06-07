%%% mnesia is probably overkill.
-module(master_wright_ban).

-export([init/0, add/1, is_banned/1, remove/1, list/0]).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(master_wright_bans,
                        [{disc_copies, [node()]}]),
    ok.

add(Ip) ->
    F = fun() -> mnesia:write({master_wright_bans, Ip, ok}) end,
    mnesia:transaction(F).

remove(Ip) ->
    F = fun() -> mnesia:delete(master_wright_bans, Ip, write) end,
    mnesia:transaction(F).

is_banned(Ip) ->
    F = fun() -> mnesia:read(master_wright_bans, Ip) end,
    case mnesia:transaction(F) of
        {atomic, List} -> {ok, List =/= []};
        Error -> Error
    end.

list() ->
    F = fun() -> mnesia:all_keys(master_wright_bans) end,
    mnesia:transaction(F).
