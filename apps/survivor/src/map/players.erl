-module(players).
-author("nt").

-export([players/0, add/2, remove/2, update/2, input/3]).

-record(player, {
  id :: integer(),
  position :: point:point(),
  movement_speed :: float(),
  destination :: point:point() | undefined,
  update_position = false :: boolean(),
  health = 0 :: integer(),
  health_regen = 0 :: non_neg_integer(),
  mana = 0 :: integer(),
  mana_regen = 0 :: non_neg_integer(),
  attack_speed = 0.0 :: float(),
  attack_range = 0.0 :: float(),
  attack_damage = 0 :: integer()
}).
-opaque player() :: #player{}.
-export_type([player/0]).

players() ->
  [].

add(Players, Id) ->
  P = point:point(300, 500),
  Player = #player{
    id = Id,
    position = P,
    movement_speed = 100.0,
    update_position = true
  },
  [Player | Players].

remove(Players, Id) ->
  lists:filter(fun(P) -> P#player.id =/= Id end, Players).

input(Players, Id, T) ->
  lists:map(fun(P) ->
    if
      P#player.id == Id -> P#player{destination = T};
      true -> P
    end
  end, Players).

update(State, Dt) ->
  Moved = lists:map(fun(P) -> move(P, Dt) end, State),
  Update = lists:filter(fun(#player{update_position = Update}) ->
    Update == true
  end, Moved),
  lists:foreach(fun(#player{id = Id, position = P}) ->
    ws_send:teleport(Id, P)
  end, Update),
  Clean = lists:map(fun(P) -> clean(P) end, Moved),
  Clean.

move(#player{destination = undefined} = Player, _) ->
  Player;
move(#player{position = A, destination = B, movement_speed = S} = Player, Dt) ->
  case pathfinding:next_point(A, B, S, Dt) of
    undefined ->
      Player#player{destination = undefined};
    New ->
      Player#player{position = New, update_position = true}
  end.

clean(Player) ->
  Player#player{update_position = false}.
