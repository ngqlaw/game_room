%%%-------------------------------------------------------------------
%% @doc game room API.
%% @end
%%%-------------------------------------------------------------------

-module(game_room).

-include("game_room.hrl").

-export([
    start_server/2,
    new/2,
    get_pid/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 启动房间进程服务
-spec (start_server(Type::term(), Handler::atom()) -> boolean()).
start_server(Type, Handler) ->
    try
        true = game_room_type_index:add(#game_room_type{type = Type}),
        RoomTab = game_room_data:create(),
        {ok, Pid} = game_room_sup:start_child([Type, Handler, RoomTab]),

        Children = supervisor:which_children(Pid),
        {_, ManagerPid, _, _} = lists:keyfind([game_room_manager], 4, Children),
        {_, SupPid, _, _} = lists:keyfind([game_room_handler_sup], 4, Children),

        true = game_room_type_index:update(#game_room_type{
            type = Type,
            pid = Pid,
            handle_sup_pid = SupPid,
            manager_pid = ManagerPid,
            table = RoomTab
        }),
        true
    catch
        _:_ ->
            game_room_type_index:delete(Type),
            false
    end.

%% @doc 新增房间
-spec (new(term(), term()) -> undefined | {ok, RoomId::integer(), Pid::pid()} | term()).
new(Type, SubType) ->
    case game_room_type_index:lookup(Type) of
        undefined ->
            undefined;
        RoomInfo ->
            game_room_manager:new(RoomInfo, SubType)
    end.

%% @doc 获取房间进程
-spec (get_pid(Type::term(), Key::integer()) -> undefined | pid()).
get_pid(Type, RoomId) ->
    case get_room_tab(Type) of
        undefined -> undefined;
        Tab -> game_room_data:lookup(Tab, RoomId)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 获取房间索引表
get_room_tab(Type) ->
    case game_room_type_index:lookup(Type) of
        undefined ->
            undefined;
        #game_room_type{table = Tab} ->
            Tab
    end.
