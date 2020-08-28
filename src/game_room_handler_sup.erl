%%%-------------------------------------------------------------------
%% @doc game room handler supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_room_handler_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Mod), {Mod, {Mod, start_link, []}, temporary, 5000, worker, [Mod]}).

%%====================================================================
%% API functions
%%====================================================================

start_link(Handler) ->
    supervisor:start_link(?MODULE, [Handler]).

start_child(Pid, Args) ->
    supervisor:start_child(Pid, [Args]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Handler]) ->
    {ok, { {simple_one_for_one, 3, 10}, [?CHILD(Handler)]} }.

%%====================================================================
%% Internal functions
%%====================================================================
