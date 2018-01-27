%%%-------------------------------------------------------------------
%% @doc game room type supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_room_type_sup).

-behaviour(supervisor).

-include("game_room.hrl").

%% API
-export([start_link/1, get_manager/1, get_handler_sup/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WORKER(Mod, Key), {{Mod, Key}, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}).
-define(SUP(Mod, Key, Handler), {{Mod, Key}, {Mod, start_link, [Handler]}, permanent, 5000, supervisor, [Mod]}).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
  supervisor:start_link(?MODULE, Args).

%% 获取房间管理进程
get_manager(Pid) ->
  Children = supervisor:which_children(Pid),
  {_, ManagerPid, _, _} = lists:keyfind([game_room_manager], 4, Children),
  ManagerPid.

%% 获取房间进程监督树进程
get_handler_sup(Pid) ->
  Children = supervisor:which_children(Pid),
  {_, SupPid, _, _} = lists:keyfind([game_room_handler_sup], 4, Children),
  SupPid.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Type, Handler]) ->
  Manager = ?WORKER(game_room_manager, Type),
  HandlerSup = ?SUP(game_room_handler_sup, Type, Handler),
  true = game_room_process:update(#game_room_type{type = Type, pid = self()}),
  {ok, { {one_for_all, 3, 10}, [Manager, HandlerSup]} }.

%%====================================================================
%% Internal functions
%%====================================================================
