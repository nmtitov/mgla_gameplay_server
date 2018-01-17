-module(players).
-author("nt").
-include("header/player_state.hrl").
-export([players/0, add/2, remove/2, update/4, handle_input/4]).

-record(player, {
  id :: integer(),
  position :: point:point(),
  movement_speed = 100 :: float(),
  path = [] :: [point:point()],
  update_position = true :: boolean(),
  health = 0 :: integer(),
  health_regen = 0 :: non_neg_integer(),
  mana = 0 :: integer(),
  mana_regen = 0 :: non_neg_integer(),
  attack_speed = 0.0 :: float(),
  attack_range = 0.0 :: float(),
  attack_damage = 0 :: integer(),
  state = idle :: player_state(),
  update_state = true :: boolean()
}).
-opaque player() :: #player{}.
-export_type([player/0]).

players() ->
  [].

add(Players, Id) ->
  {ok, P} = path_server:initial_point(),
  Player = #player{
    id = Id,
    position = P
  },
  [Player | Players].

remove(Players, Id) ->
  lists:filter(fun(P) -> P#player.id =/= Id end, Players).

handle_input(Players, Id, T, Blocks) ->
  lists:map(fun(P) ->
    if
      P#player.id == Id ->
        {ok, Path} = path_server:path(P#player.position, T, Blocks),
        P#player{path = Path};
      true -> P
    end
  end, Players).

update(State, Dt, MapRect, Blocks) ->
  NewState = lists:map(fun(P) -> move(P, Dt, MapRect, Blocks) end, State),
  Updated = lists:filter(fun(#player{update_position = UpdatePosition, update_state = UpdateState}) ->
    (UpdatePosition == true) or (UpdateState == true)
  end, NewState),
  lists:foreach(fun(#player{id = Id, position = P, update_position = UpdatePosition, state = State, update_state = UpdateState}) ->
    State2 = if
               UpdateState == true -> State;
               true -> undefined
             end,
    ws_send:broadcast_update(Id, P, State2)
  end, Updated),
  lists:map(fun(P) -> clean(P) end, NewState).

move(#player{path = [], state = State} = Player, _, _, _) ->
  case State of
    walk -> Player#player{state = idle, update_state = true};
    _ -> Player
  end;
move(#player{position = A, path = [B|Tail], movement_speed = S, state = State} = Player, Dt, MapRect, Blocks) ->
  case path_server:next_point(A, B, S, Dt, MapRect, Blocks) of
    {ok, undefined} ->
      Player#player{path = Tail};
    {ok, New} ->
      case State of
        idle -> Player#player{position = New, update_position = true, state = walk, update_state = true};
        _ -> Player#player{position = New, update_position = true}
      end
  end.

clean(Player) ->
  Player#player{
    update_position = false,
    update_state = false
  }.
