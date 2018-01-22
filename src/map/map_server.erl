-module(map_server).
-author("nt").

-behaviour(gen_server).

-define(UPDATE_RATE, 33).

-export([start_link/0, enter/1, leave/1, add_bot/1, remove_bot/1]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-spec enter(non_neg_integer()) -> ok.
enter(Id) ->
  gen_server:cast(?SERVER, {enter, Id}).

-spec leave(non_neg_integer()) -> ok.
leave(Id) ->
  gen_server:cast(?SERVER, {leave, Id}).

-spec add_bot(Id) -> ok when Id :: non_neg_integer().
add_bot(Id) ->
  gen_server:cast(?SERVER, {add_bot, Id}).

-spec remove_bot(Id) -> ok when Id :: non_neg_integer().
remove_bot(Id) ->
  gen_server:cast(?SERVER, {remove_bot, Id}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Callbacks

init(Params) ->
  lager:info("~p:~p ~p:~p/~p(~p)", [?FILE, ?LINE, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Params]),
  Blocks = map_tools:blocks(),
  State = #{
    rect => {{0, 0}, {600, 1000}},
    players => [],
    bots => [],
    blocks => Blocks
  },
  {ok, State, 0}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({enter, Id} = Message, #{players := PlayerIds} = State) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  NewState = State#{players := [Id| PlayerIds]},
  ws_send:send_map(Id),
  lager:info("id=~p", [Id]),
  ws_send:broadcast_enter(Id),
  {noreply, NewState};
handle_cast({leave, Id}, #{players := PlayerIds} = State) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  NewPlayers = lists:filter(fun(Id2) -> Id =/= Id2 end, PlayerIds),
  ws_send:broadcast_leave(Id),
  NewState = State#{players := NewPlayers},
  {noreply, NewState};
handle_cast({add_bot, Id}, #{bots := BotIds} = State) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  NewState = State#{bots := [Id| BotIds]},
  ws_send:broadcast_enter(Id),
  {noreply, NewState};
handle_cast({remove_bot, Id}, #{bots := BotIds} = State) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  NewBotIds = lists:filter(fun(Id2) -> Id =/= Id2 end, BotIds),
  NewState = State#{bots := NewBotIds},
  ws_send:broadcast_leave(Id),
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({timeout, _Ref, update} = Message, #{rect := MapRect, players := PlayerIds, bots := BotIds, blocks := Blocks} = State) ->
%%  lager:info("~p:~p ~p:~p/~p(~p, ~p)", [?FILE, ?LINE, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Message, State]),

  TimeA = erlang:system_time(),

  Players = lists:map(fun(Id) -> avatar_server:get_state(Id) end, PlayerIds),
  NewPlayers = update(Players, ?UPDATE_RATE / 1000.0, MapRect, Blocks),

  lists:foreach(fun(#{id := Id} = Player) ->
    avatar_server:set_state(Id, Player)
  end, NewPlayers),

  Bots = lists:map(fun(Id) -> bot_server:get_state(Id) end, BotIds),
  update(Bots),

  TimeB = erlang:system_time(),
  _ = TimeB - TimeA,
%%  lager:info("TimeDelta=~p", [TimeDelta]),
  schedule_update(),
  {noreply, State};
handle_info(timeout, State) ->
  lager:info("~p:~p ~p:~p/~p(~p, State)", [?FILE, ?LINE, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, timeout]),
  schedule_update(),
  {noreply, State};
handle_info(Info, State) ->
  lager:info("~p:~p ~p:~p/~p(~p = Info, State)", [?FILE, ?LINE, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, timeout]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private

schedule_update() ->
  erlang:start_timer(?UPDATE_RATE, self(), update).

update(Players, Dt, MapRect, Blocks) ->
%%  PlayersToMove = lists:filter(fun(Player) -> m
  MovedPlayers = lists:map(fun(Player) -> move(Player, Dt, MapRect, Blocks) end, Players),
  Updated = lists:filter(fun(#{position := #{update := UpdatePosition}, state := #{update := UpdateState}}) ->
    (UpdatePosition == true) or (UpdateState == true)
  end, MovedPlayers),
  lists:foreach(fun(Player) ->
    Id = avatar:get_id(Player),
    P = avatar:get_position_value(Player),
    State = avatar:get_state_value(Player),
    UpdateState = avatar:get_state_update(Player),
    PlayerState2 = if
      UpdateState == true -> State;
      true -> undefined
    end,
    ws_send:broadcast_update(Id, P, PlayerState2)
  end, Updated),
  lists:map(fun(P) -> avatar:clear_update_flags(P) end, MovedPlayers).

move(#{path := [], state := #{value := State}} = Player, _, _, _) ->
%%  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  case State of
    walk -> avatar:set_state_value(idle, Player);
    _    -> Player
  end;
move(#{id := Id, position := #{value := A}, path := [B|Rest], movement_speed := S, state := #{value := State}} = Player, Dt, MapRect, Blocks) ->
%%  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
%%  lager:info("~p", [Player]),
  case pathfinder_server:next_point(Id, A, B, S, Dt, MapRect, Blocks) of
    undefined ->
%%      lager:info("Next point is undefined"),
      avatar:set_path(Rest, Player);
    New ->
%%      lager:info("Next point is = ~p", [New]),
%%      lager:info("State is = ~p", [State]),
      case State of
        idle ->
          NewPlayer = avatar:set_position_value(New, Player),
          NewPlayer2 = avatar:set_state_value(walk, NewPlayer),
%%          lager:info("New player (state, position updated) = ~p", [NewPlayer2]),
          NewPlayer2;
        _ ->
          NewPlayer = avatar:set_position_value(New, Player),
%%          lager:info("New player (position updated) = ~p", [NewPlayer]),
          NewPlayer
      end
  end.

update(Bots) ->
  Updated = lists:filter(fun(#{position := #{update := UpdatePosition}, state := #{update := UpdateState}}) ->
    (UpdatePosition == true) or (UpdateState == true)
  end, Bots),
  lists:foreach(fun(A) ->
    Id = avatar:get_id(A),
    P = avatar:get_position_value(A),
    State = avatar:get_state_value(A),
    UpdateState = avatar:get_state_update(A),
    AState = if
     UpdateState == true -> State;
     true -> undefined
    end,
    ws_send:broadcast_update(Id, P, AState)
  end, Updated),
  lists:map(fun(P) -> avatar:clear_update_flags(P) end, Updated).