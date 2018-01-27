%%%-------------------------------------------------------------------
%% @doc game room manager.
%% @end
%%%-------------------------------------------------------------------

-module(game_room_manager).

-behaviour(gen_server).

-include("game_room.hrl").

%% API
-export([start_link/0]).
-export([new/3, get_room_tab/1]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  room_tab = undefined :: term(),
  player_tab = undefined :: term()
  }).

%%%===================================================================
%%% API
%%%===================================================================
%% 创建进程
-spec (new(pid(), pid(), term()) -> {integer(), pid()} | term()).
new(Pid, TopPid, SubType) ->
  gen_server:call(Pid, {new, TopPid, SubType}).

%% 获取房间进程索引表
-spec (get_room_tab(pid()) -> term()).
get_room_tab(Pid) ->
  gen_server:call(Pid, get_room_tab).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  RoomTab = game_room_data:create(),
  PlayerTab = game_room_player:create(),
  {ok, #state{room_tab = RoomTab, player_tab = PlayerTab}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
  State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({new, TopPid, Type}, _From, #state{room_tab = Tab} = State) ->
  try
    RoomId = game_room_data:add(Tab),
    SupPid = game_room_type_sup:get_handler_sup(TopPid),
    {ok, Pid} = game_room_handler_sup:start_child(SupPid, [RoomId, Type]),
    erlang:monitor(process, Pid),
    erlang:put({room, Pid}, RoomId),
    game_room_data:insert(Tab, #game_room{
      id = RoomId, 
      num = 0, 
      pid = Pid, 
      type = Type
    }),
    {reply, {RoomId, Pid}, State}
  catch
    E:R ->
      {reply, {E, R, erlang:get_stacktrace()}, State}
  end;
handle_call(get_room_tab, _From, #state{room_tab = Tab} = State) ->
  {reply, Tab, State};
handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({'DOWN', _Ref, process, Object, _Reason}, #state{room_tab = Tab} = State) ->
  RoomId = erlang:erase({room, Object}),
  game_room_data:delete(Tab, RoomId),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
  State :: #state{}) -> term()).
terminate(_Reason, #state{room_tab = RoomTab, player_tab = PlayerTab} = _State) ->
  try
    game_room_data:clear(RoomTab),
    game_room_player:clear(PlayerTab),
    ok
  catch
    _E:_R ->
      ok
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
  Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
