-module(players).
-author("nt").

-export([add/2, remove/2, update/2, input/3]).

-record(player, {
  id :: integer(),
  position :: point:point(),
  speed :: float(),
  target :: point:point() | undefined,
  update_position = false :: boolean()
}).
-opaque player() :: #player{}.
-export_type([player/0]).

add(Players, Id) ->
  P = point:point(0, 0),
  Player = #player{id = Id, position = P, speed = 100.0, update_position = true},
  [Player | Players].

remove(Players, Id) ->
  lists:filter(fun(P) -> P#player.id =/= Id end, Players).

input(Players, Id, T) ->
  lists:map(fun(P) ->
    if
      P#player.id == Id -> P#player{target = T};
      true -> P
    end
  end, Players).

move(#player{position = P, target = T, speed = S} = Player, Dt) ->
  case pathfinding:next_point(P, T, S, Dt) of
    undefined ->
      Player#player{target = undefined};
    New ->
      Player#player{position = New, update_position = true}
  end.

update(State, Dt) ->
  State2 = lists:map(fun(P) -> move(P, Dt) end, State),
  Update = lists:filter(fun(#player{update_position = Update}) ->
    Update == true
  end, State2),
  lists:foreach(fun(#player{id = Id, position = P}) ->
    ws_send:teleport(Id, P)
  end, Update),
  State3 = lists:map(fun(Player) ->
    Player#player{update_position = false}
  end, State2),
  State3.

