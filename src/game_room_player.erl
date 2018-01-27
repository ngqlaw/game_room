%%%-------------------------------------------------------------------
%% @doc game room player public info.
%% @end
%%%-------------------------------------------------------------------

-module(game_room_player).

-export([
  create/0
  ,clear/1
  ,lookup/2
  ,insert/3
  ,delete/2
]).

%% 创建房间玩家信息表
-spec (create() -> term()).
create() ->
  ets:new(game_room_player, [
    public, 
    {keypos, 1}, 
    {write_concurrency, true}, 
    {read_concurrency, true}
  ]).

%% 删除信息表
-spec (clear(Tab::term()) -> true).
clear(Tab) ->
  ets:delete(Tab).

%% 查询房间玩家信息
-spec (lookup(Tab::term(), Key::term()) -> undefined | term()).
lookup(Tab, Key) ->
  case ets:lookup(Tab, Key) of
    [{_, Value}] -> 
      Value;
    _ -> 
      undefined
  end.

%% 更新玩家信息
-spec (insert(Tab::term(), Key::term(), Value::term()) -> true).
insert(Tab, Key, Value) ->
  ets:insert(Tab, {Key, Value}).

%% 移除玩家
-spec (delete(Tab::term(), Key::term()) -> true).
delete(Tab, Key) ->
  ets:delete(Tab, Key).
