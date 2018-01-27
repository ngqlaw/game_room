%%%-------------------------------------------------------------------
%% @doc game room API.
%% @end
%%%-------------------------------------------------------------------

-module(game_room).

-include("game_room.hrl").

-export([
  start_server/2
  ,new/2
  ,select/2
  ,select_n/2
  ,select_n/3
  ,insert/2
  ,delete/2
  ,incr/3
  ,get_pid/2
]).

%% 启动房间进程服务
-spec (start_server(Type::term(), Handler::atom()) -> boolean()).
start_server(Type, Handler) ->
  case game_room_process:add(#game_room_type{type = Type}) of
    true ->
      {ok, _} = game_room_sup:start_child([Type, Handler]),
      true;
    false ->
      false
  end.

%% 新增房间
-spec (new(term(), term()) -> undefined | {RoomId::integer(), Pid::pid()} | term()).
new(Type, SubType) ->
  case game_room_process:lookup(Type) of
    undefined ->
      undefined;
    Pid -> 
      ManagerPid = game_room_type_sup:get_manager(Pid),
      game_room_manager:new(ManagerPid, Pid, SubType)
  end.

%% 获取符合条件的房间列表
%% 例如：查询所有有人的房间 Cond :: fun(#game_room{id = Id, num = Num}) when Num > 0 -> Id end.
-spec (select(Type::term(), Cond::function()) -> list()).
select(Type, Cond) when is_function(Cond) ->
  case get_room_tab(Type) of
    undefined -> [];
    Tab -> game_room_data:select(Tab, Cond)
  end.

%% 获取指定数量的符合条件的房间(可以传入继续查询指针进行指定数量的符合条件房间查询)
-spec (select_n(Type::term(), Continuation::term()) -> none | {list(), term()}).
select_n(Type, Continuation) ->
  case get_room_tab(Type) of
    undefined -> none;
    Tab -> game_room_data:select_n(Tab, Continuation)
  end.
-spec (select_n(Type::term(), Cond::function(), N::integer()) -> none | {list(), term()}).
select_n(Type, Cond, N) when is_function(Cond) andalso is_integer(N) ->
  case get_room_tab(Type) of
    undefined -> none;
    Tab -> game_room_data:select_n(Tab, Cond, N)
  end.

%% 新增索引
-spec (insert(Type::term(), Info::tuple()) -> boolean()).
insert(Type, Info) ->
  case get_room_tab(Type) of
    undefined -> false;
    Tab -> game_room_data:insert(Tab, Info)
  end.

%% 移除房间
-spec (delete(Type::term(), Key::integer()) -> boolean()).
delete(Type, RoomId) ->
  case get_room_tab(Type) of
    undefined -> false;
    Tab -> game_room_data:delete(Tab, RoomId)
  end.

%% 更新房间人数
-spec (incr(Type::term(), Key::integer(), Incr::integer()) -> undefined | integer()).
incr(Type, RoomId, Incr) ->
  case get_room_tab(Type) of
    undefined -> undefined;
    Tab -> game_room_data:incr(Tab, RoomId, Incr)
  end.

%% 获取房间进程信息
-spec (get_pid(Type::term(), Key::integer()) -> undefined | {pid(), integer()}).
get_pid(Type, RoomId) ->
  case get_room_tab(Type) of
    undefined -> undefined;
    Tab -> game_room_data:lookup(Tab, RoomId)
  end.

%% 获取房间索引表
get_room_tab(Type) ->
  case game_room_process:lookup(Type) of
    undefined ->
      undefined;
    Pid ->
      ManagerPid = game_room_type_sup:get_manager(Pid),
      game_room_manager:get_room_tab(ManagerPid)
  end.
