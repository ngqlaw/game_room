%%%-------------------------------------------------------------------
%% @doc game_room top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_room_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WORKER(Mod, Key, Args), {{Mod, Key}, {Mod, start_link, [Args]}, permanent, 5000, worker, [Mod]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child([Key|_] = Args) ->
  supervisor:start_child(?SERVER, ?WORKER(game_room_type_sup, Key, Args)).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  game_room_process:create(),
  {ok, { {one_for_one, 3, 10}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
