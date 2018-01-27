%%%-------------------------------------------------------------------
%% @doc game room process API.
%% @end
%%%-------------------------------------------------------------------

-module(game_room_process).

-include("game_room.hrl").

-export([
  create/0
  ,lookup/1
  ,add/1
  ,update/1
  ,delete/1
]).

-define(TABLE, game_room_processes_index).

%% 创建房间进程索引表
-spec (create() -> ok).
create() ->
  ets:new(?TABLE, [
    named_table, 
    public, 
    {keypos, 2}, 
    {write_concurrency, true}, 
    {read_concurrency, true}
  ]),
  ok.

%% 查询房间进程
-spec (lookup(term()) -> undefined | pid()).
lookup(Key) ->
  case ets:lookup(?TABLE, Key) of
    [#game_room_type{pid = Pid}] -> 
      Pid;
    _ -> 
      undefined
  end.

%% 新增索引
-spec (add(term()) -> boolean()).
add(#game_room_type{} = Info) ->
  ets:insert_new(?TABLE, Info).

%% 更新索引
-spec (update(term()) -> true).
update(#game_room_type{} = Info) ->
  ets:insert(?TABLE, Info).

%% 移除房间进程索引
-spec (delete(term()) -> true).
delete(Key) ->
  ets:delete(?TABLE, Key).
