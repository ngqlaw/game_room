%%%-------------------------------------------------------------------
%% @doc game room data.
%% @end
%%%-------------------------------------------------------------------

-module(game_room_data).

-include("game_room.hrl").

-export([
    create/0,
    clear/1,
    lookup/2,
    insert/2,
    add/1,
    delete/2
]).

-define(ROOM_ID, room_index).

%% 创建房间索引表
-spec (create() -> term()).
create() ->
    Table = ets:new(game_room, [public, {keypos, 2}, {write_concurrency, true}, {read_concurrency, true}]),
    insert(Table, ?ROOM_ID, []),
    Table.

%% 删除索引表
-spec (clear(term()) -> true).
clear(Tab) ->
    ets:delete(Tab).

%% 查询房间索引信息
-spec (lookup(Table::term(), Key::term()) -> undefined | term()).
lookup(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [{_, _, Value}] -> 
            Value;
        [#game_room{pid = Pid}] -> 
            Pid;
        _ -> 
            undefined
    end.

%% 新增索引
-spec (insert(Table::term(), tuple()) -> true).
insert(Tab, Info) ->
    ets:insert(Tab, Info).
insert(Tab, Key, Value) ->
    ets:insert(Tab, {table, Key, Value}).

%% 增加房间(同一个进程中顺序调用)
-spec (add(term()) -> integer()).
add(Tab) ->
    IndexList = lookup(Tab, ?ROOM_ID),
    {RoomId, NewIndexList} = do_add(IndexList, 1, []),
    insert(Tab, ?ROOM_ID, NewIndexList),
    RoomId.

do_add([Id|T], Id, Res) -> do_add(T, Id + 1, Res);
do_add([H|T], Id, Res) -> {Id, lists:reverse([H|Res], [Id|T])};
do_add([], Id, Res) -> {Id, lists:reverse([Id|Res])}.

%% 移除房间
-spec (delete(Tab::term(), Key::integer()) -> true).
delete(Tab, RoomId) ->
    IndexList = lookup(Tab, ?ROOM_ID),
    NewIndexList = lists:delete(RoomId, IndexList),
    insert(Tab, ?ROOM_ID, NewIndexList),
    ets:delete(Tab, RoomId).
