-module(map_server).
-author("nt").

-behaviour(gen_server).

-define(UPDATE_RATE, 33).

-export([start_link/0, enter/1, leave/1]).

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

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Callbacks

init(Params) ->
  lager:info("map_server:init(~p)", [Params]),
  Blocks = map_tools:blocks(),
  State = #{
    rect => {{0, 0}, {600, 1000}},
    players => [],
    blocks => Blocks
  },
  {ok, State, 0}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({enter, Id}, #{players := PlayerIds} = State) ->
  NewState = State#{players => [Id| PlayerIds]},
  ws_send:send_map(Id),
  ws_send:broadcast_enter(Id),
  {noreply, NewState};
handle_cast({leave, Id}, #{players := PlayerIds} = State) ->
  NewPlayers = lists:filter(fun(Id2) -> Id =/= Id2 end, PlayerIds),
  ws_send:broadcast_leave(Id),
  NewState = State#{players => NewPlayers},
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({timeout, _Ref, update}, #{rect := MapRect, players := PlayerIds, blocks := Blocks} = State) ->
%%  lager:info("handle_info({timeout, _Ref, update}"),
  TimeA = erlang:system_time(),

  Players = lists:map(fun(Id) -> avatar_server:get_state(Id) end, PlayerIds),
  NewPlayers = update(Players, ?UPDATE_RATE / 1000.0, MapRect, Blocks),

  lists:foreach(fun(#{id := Id} = Player) ->
    avatar_server:set_state(Id, Player)
  end, NewPlayers),

  TimeB = erlang:system_time(),
  _ = TimeB - TimeA,
%%  lager:info("TimeDelta=~p", [TimeDelta]),
  schedule_update(),
  {noreply, State};
handle_info(timeout, State) ->
  lager:info("map_server:handle_info(timeout)"),
  schedule_update(),
  {noreply, State};
handle_info(_Info, State) ->
  lager:info("map_server:handle_info(_Info)"),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private

schedule_update() ->
  erlang:start_timer(?UPDATE_RATE, self(), update).

update(Players, Dt, MapRect, Blocks) ->
  MovedPlayers = lists:map(fun(Player) -> move(Player, Dt, MapRect, Blocks) end, Players),
  Updated = lists:filter(fun(#{update_position := UpdatePosition, update_state := UpdateState}) ->
    (UpdatePosition == true) or (UpdateState == true)
  end, MovedPlayers),
  lists:foreach(fun(#{id := Id, position := P, update_position := _, state := State, update_state := UpdateState}) ->
    PlayerState2 = if
      UpdateState == true -> State;
      true -> undefined
    end,
    ws_send:broadcast_update(Id, P, PlayerState2)
  end, Updated),
  lists:map(fun(P) -> clean(P) end, MovedPlayers).

move(#{path := [], state := State} = Player, _, _, _) ->
  case State of
    walk -> Player#{state => idle, update_state => true};
    _ -> Player
  end;
move(#{id := Id, position := A, path := [B|Rest], movement_speed := S, state := State} = Player, Dt, MapRect, Blocks) ->
  case pathfinder_server:next_point(Id, A, B, S, Dt, MapRect, Blocks) of
    undefined ->
      Player#{path => Rest};
    New ->
      case State of
        idle -> Player#{position => New, update_position => true, state => walk, update_state => true};
        _ -> Player#{position => New, update_position => true}
      end
  end.

clean(Player) ->
  Player#{
    update_position => false,
    update_state => false
  }.