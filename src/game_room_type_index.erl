%%%-------------------------------------------------------------------
%% @doc game room type index API.
%% @end
%%%-------------------------------------------------------------------

-module(game_room_type_index).

-include("game_room.hrl").

-export([
    create/0
    ,lookup/1
    ,add/1
    ,update/1
    ,delete/1
]).

-type(room_info() :: #game_room_type{}).

%% 创建房间进程索引表
-spec (create() -> ok).
create() ->
    ets:new(?MODULE, [named_table, public, {keypos, 2},
        {write_concurrency, true}, {read_concurrency, true}]),
    ok.

%% 查询房间
-spec (lookup(term()) -> undefined | room_info()).
lookup(Key) ->
    case ets:lookup(?MODULE, Key) of
        [#game_room_type{} = Value] -> 
            Value;
        _ -> 
            undefined
    end.

%% 新增索引
-spec (add(term()) -> boolean()).
add(#game_room_type{} = Info) ->
    ets:insert_new(?MODULE, Info).

%% 更新索引
-spec (update(term()) -> true).
update(#game_room_type{} = Info) ->
    ets:insert(?MODULE, Info).

%% 移除房间进程索引
-spec (delete(term()) -> true).
delete(Key) ->
    ets:delete(?MODULE, Key).
