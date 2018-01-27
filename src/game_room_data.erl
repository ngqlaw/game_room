%%%-------------------------------------------------------------------
%% @doc game room data.
%% @end
%%%-------------------------------------------------------------------

-module(game_room_data).

-include("game_room.hrl").

-export([
  create/0
  ,clear/1
  ,lookup/2
  ,insert/2
  ,add/1
  ,delete/2
  ,incr/3
  ,select/2
  ,select_n/2
  ,select_n/3
]).

-define(ROOM_ID, room_index).

%% 创建房间索引表
-spec (create() -> term()).
create() ->
  Table = ets:new(game_room, [
    public, 
    {keypos, 2}, 
    {write_concurrency, true}, 
    {read_concurrency, true}
  ]),
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
    [#game_room{
      num = Num, 
      pid = Pid
    }] -> 
      {Pid, Num};
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

%% 更新房间人数
-spec (incr(Tab::term(), Key::integer(), Incr::integer()) -> integer()).
incr(Tab, RoomId, Incr) ->
  ets:update_counter(Tab, RoomId, {#game_room.num, Incr}).

%% 获取符合条件的房间列表
%% 例如：查询所有有人的房间 Cond :: fun(#game_room{id = Id, num = Num}) when Num > 0 -> Id end.
-spec (select(Tab::term(), Cond::function()) -> list()).
select(Tab, Cond) when is_function(Cond) ->
  Ms = ets:fun2ms(Cond),
  ets:select(Tab, Ms).

%% 获取指定数量的符合条件的房间(可以传入继续查询指针进行指定数量的符合条件房间查询)
-spec (select_n(Tab::term(), Continuation::term()) -> none | {list(), term()}).
select_n(Tab, Continuation) ->
  case ets:select(Tab, Continuation) of
    '$end_of_table' -> none;
    Res -> Res
  end.
-spec (select_n(Tab::term(), Cond::function(), N::integer()) -> none | {list(), term()}).
select_n(Tab, Cond, N) when is_function(Cond) andalso is_integer(N) ->
  Ms = ets:fun2ms(Cond),
  case ets:select(Tab, Ms, N) of
    '$end_of_table' -> none;
    Res -> Res
  end.
