-module(map_server).
-author("nt").

-behaviour(gen_server).

-define(UPDATE_RATE, 33).

-export([start_link/0, add_avatar/2, remove_avatar/1]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-spec add_avatar(Type, Id) -> ok when Type :: atom(), Id :: id_server:id().
add_avatar(Type, Id) ->
  gen_server:cast(?SERVER, {add_avatar, Type, Id}).

-spec remove_avatar(id_server:id()) -> ok.
remove_avatar(Id) ->
  gen_server:cast(?SERVER, {remove_avatar, Id}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Callbacks

init(Params) ->
  lager:info("~p:~p ~p:~p/~p(~p)", [?FILE, ?LINE, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Params]),
  Blocks = map_tools:blocks(),
  State = #{
    rect => {{0, 0}, {600, 1000}},
    avatars => [],
    blocks => Blocks
  },
  {ok, State, 0}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({add_avatar, Type, Id}, #{avatars := AvatarsMeta} = State) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  NewAvatarsMeta = [{Type, Id}| AvatarsMeta],
  NewState = State#{avatars := NewAvatarsMeta},

  case Type of
    player ->
      ws_send:send_map(Id),
      Avatars = lists:map(fun({Type, Id2}) ->
        case Type of
          bot  -> bot_server:get_state(Id2);
          player -> avatar_server:get_state(Id2)
        end
      end, AvatarsMeta),
      lists:foreach(fun(A) ->
        AvatarId = avatar:get_id(A),
        AvatarPosition = avatar:get_position_value(A),
        AvatarState = avatar:get_state_value(A),
        ws_send:send_update(Id, AvatarId, AvatarPosition, AvatarState)
      end, Avatars);
    _ -> ok
  end,

  ws_send:broadcast_enter(Id),

  {noreply, NewState};

handle_cast({remove_avatar, Id}, #{avatars := AvatarsMeta} = State) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  NewAvatarsMeta = lists:filter(fun({_, Id2}) -> Id =/= Id2 end, AvatarsMeta),
  NewState = State#{avatars := NewAvatarsMeta},
  factory_sup:stop_child(Id),
  ws_send:broadcast_leave(Id),
  {noreply, NewState};

handle_cast(_Request, State) ->
  {noreply, State}.


handle_info({timeout, _Ref, update} = Message, State) ->
%%  lager:info("~p:~p/~p(~p, ~p)", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Message, State]),
  NewState = update(State),
  {noreply, NewState};

handle_info(timeout, State) ->
  lager:info("~p:~p/~p(~p, State)", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, timeout]),
  start_bots(),
  NewState = update(State),
  {noreply, NewState};

handle_info(Info, State) ->
  lager:info("~p:~p/~p(~p = Info, State)", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Info]),
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Private

schedule_update() ->
  erlang:start_timer(?UPDATE_RATE, self(), update).

update(#{rect := MapRect, avatars := AvatarsMeta, blocks := Blocks} = State) ->
  TimeA = erlang:system_time(),

  Avatars = lists:map(fun({Type, Id}) ->
    case Type of
      bot  -> bot_server:get_state(Id);
      player -> avatar_server:get_state(Id)
    end
  end, AvatarsMeta),
  Dt = ?UPDATE_RATE / 1000.0,
  NewAvatars = update(Avatars, Dt, MapRect, Blocks),

  lists:foreach(fun(#{id := Id, type := Type} = Avatar) ->
    case Type of
      bot  -> bot_server:set_state(Id, Avatar);
      player -> avatar_server:set_state(Id, Avatar)
    end
  end, NewAvatars),

  TimeB = erlang:system_time(),
  _ = TimeB - TimeA,
%%  lager:info("TimeDelta=~p", [TimeDelta]),
  schedule_update(),
  State.

update(Avatars, Dt, MapRect, Blocks) ->
  MovedAvatars = lists:map(fun(Player) -> move(Player, Dt, MapRect, Blocks) end, Avatars),

  Updated = lists:filter(fun(#{position := #{update := UpdatePosition}, state := #{update := UpdateState}}) ->
    (UpdatePosition == true) or (UpdateState == true)
  end, MovedAvatars),
  lists:foreach(fun(Avatar) ->
    Id = avatar:get_id(Avatar),
    P = avatar:get_position_value(Avatar),
    State = avatar:get_state_value(Avatar),
    UpdateState = avatar:get_state_update(Avatar),
    PlayerState2 = if
      UpdateState == true -> State;
      true -> undefined
    end,
    ws_send:broadcast_update(Id, P, PlayerState2)
  end, Updated),

  lists:map(fun(P) -> avatar:clear_update_flags(P) end, MovedAvatars).

move(#{path := [], state := #{value := State}} = Avatar, _, _, _) ->
%%  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  case State of
    walk -> avatar:set_state_value(idle, Avatar);
    _    -> Avatar
  end;
move(#{id := Id, position := #{value := A}, path := [B|Rest], movement_speed := S, state := #{value := State}} = Avatar, Dt, MapRect, Blocks) ->
%%  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
%%  lager:info("~p", [Player]),
  case pathfinder_server:next_point(Id, A, B, S, Dt, MapRect, Blocks) of
    undefined ->
%%      lager:info("Next point is undefined"),
      avatar:set_path(Rest, Avatar);
    New ->
%%      lager:info("Next point is = ~p", [New]),
%%      lager:info("State is = ~p", [State]),
      case State of
        idle ->
          NewPlayer = avatar:set_position_value(New, Avatar),
          NewPlayer2 = avatar:set_state_value(walk, NewPlayer),
%%          lager:info("New player (state, position updated) = ~p", [NewPlayer2]),
          NewPlayer2;
        _ ->
          NewPlayer = avatar:set_position_value(New, Avatar),
%%          lager:info("New player (position updated) = ~p", [NewPlayer]),
          NewPlayer
      end
  end.

start_bots() ->
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()),
  bot_sup:start_child(id_server:get_id()).
