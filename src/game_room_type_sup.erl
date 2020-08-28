%%%-------------------------------------------------------------------
%% @doc game room type supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_room_type_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Type, Handler, RoomTab]) ->
    Manager = {
        {game_room_manager, Type},
        {game_room_manager, start_link, [RoomTab]},
        permanent, 5000, worker, [game_room_manager]
    },
    HandlerSup = {
        {game_room_handler_sup, Type},
        {game_room_handler_sup, start_link, [Handler]},
        permanent, 5000, supervisor, [game_room_handler_sup]
    },
    {ok, { {one_for_all, 3, 10}, [Manager, HandlerSup]} }.

%%====================================================================
%% Internal functions
%%====================================================================
