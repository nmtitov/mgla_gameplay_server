-module(map_server).
-author("nt").

-behaviour(gen_server).

-define(TICK_RATE, 33).

-export([start_link/0, enter/1, leave/1, input/2]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(player_state, {
  id :: integer(),
  position :: point:point(),
  speed :: float(),
  target :: point:point() | undefined,
  should_send_teleport = false :: boolean()
}).

-spec enter(non_neg_integer()) -> ok.
enter(Id) ->
  gen_server:cast(?SERVER, {enter, Id}),
  ok.

-spec leave(non_neg_integer()) -> ok.
leave(Id) ->
  gen_server:cast(?SERVER, {leave, Id}),
  ok.

-spec input(non_neg_integer(), point:point()) -> ok.
input(Id, P) ->
  gen_server:cast(?SERVER, {input, Id, P}),
  ok.

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(init(Args :: term()) ->
  {ok, State :: [#player_state{}]} | {ok, State :: [#player_state{}], timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, [], 0}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: [#player_state{}]) ->
  {reply, Reply :: term(), NewState :: [#player_state{}]} |
  {reply, Reply :: term(), NewState :: [#player_state{}], timeout() | hibernate} |
  {noreply, NewState :: [#player_state{}]} |
  {noreply, NewState :: [#player_state{}], timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: [#player_state{}]} |
  {stop, Reason :: term(), NewState :: [#player_state{}]}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: [#player_state{}]) ->
  {noreply, NewState :: [#player_state{}]} |
  {noreply, NewState :: [#player_state{}], timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: [#player_state{}]}).
handle_cast({enter, Id}, State) ->
  P = point:point(0, 0),
  PlayerState = #player_state{id = Id, position = P, speed = 100.0, should_send_teleport = true},
  NewState = [PlayerState | State],
  {noreply, NewState};
handle_cast({leave, Id}, State) ->
  NewState = lists:filter(fun(Player) -> Player#player_state.id =/= Id end, State),
  {noreply, NewState};
handle_cast({input, Id, P}, State) ->
  NewState = lists:map(fun(Player) ->
    if
      Player#player_state.id == Id -> Player#player_state{target = P};
      true -> Player
    end
  end, State),
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: [#player_state{}]) ->
  {noreply, NewState :: [#player_state{}]} |
  {noreply, NewState :: [#player_state{}], timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: [#player_state{}]}).
handle_info({timeout, _Ref, tick}, State) ->
  TimeA = erlang:system_time(),
  State2 = lists:map(fun(#player_state{position = P, speed = S, target = T} = Player) ->
    case T of
       undefined ->
         Player;
       T ->
         case pathfinding:next_point(P, T, ?TICK_RATE / 1000.0, S) of
           undefined ->
             Player#player_state{target = undefined};
           New ->
             Player#player_state{position = New, should_send_teleport = true}
         end
    end
  end, State),
  PlayersToNotify = lists:filter(fun(#player_state{should_send_teleport = Should}) ->
    Should == true
  end, State2),
  lists:foreach(fun(#player_state{id = Id, position = P} = Player) ->
    ws_send:teleport(Id, P)
  end, PlayersToNotify),
  State3 = lists:map(fun(Player) ->
    Player#player_state{should_send_teleport = false}
  end, State2),
  TimeB = erlang:system_time(),
  TimeDiff = TimeB - TimeA,
  schedule_next_tick(),
  {noreply, State3};
handle_info(timeout, State) ->
  schedule_next_tick(),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: [#player_state{}]) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: [#player_state{}],
    Extra :: term()) ->
  {ok, NewState :: [#player_state{}]} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

schedule_next_tick() ->
  erlang:start_timer(?TICK_RATE, self(), tick).
