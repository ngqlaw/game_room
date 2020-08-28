%%%-------------------------------------------------------------------
%% @doc game_room public API
%% @end
%%%-------------------------------------------------------------------

-module(game_room_app).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start() -> start(game_room).

start(App) ->
    start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

start(_StartType, _StartArgs) ->
    game_room_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
