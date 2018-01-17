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
  P = path:initial_point(),
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
        Path = path:destination_point(P#player.position, T, Blocks),
        P#player{path = Path};
      true -> P
    end
  end, Players).

update(State, Dt, MapRect, Blocks) ->
  Moved = lists:map(fun(P) -> move(P, Dt, MapRect, Blocks) end, State),
  Update = lists:filter(fun(#player{update_position = Update}) ->
    Update == true
  end, Moved),
  lists:foreach(fun(#player{id = Id, position = P}) ->
    ws_send:broadcast_update(Id, P)
  end, Update),
  lists:map(fun(P) -> clean(P) end, Moved).

move(#player{path = []} = Player, _, _, _) ->
  Player;
move(#player{position = A, path = [B|Tail], movement_speed = S} = Player, Dt, MapRect, Blocks) ->
  case path:next_point(A, B, S, Dt, MapRect, Blocks) of
    undefined ->
      Player#player{path = Tail};
    New ->
      Player#player{position = New, update_position = true}
  end.

clean(Player) ->
  Player#player{update_position = false}.
