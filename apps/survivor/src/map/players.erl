-module(players).
-author("nt").

-export([players/0, add/2, remove/2, update/2, input/3]).

-record(player, {
  id :: integer(),
  position :: point:point(),
  speed :: float(),
  target :: point:point() | undefined,
  update_position = false :: boolean()
}).
-opaque player() :: #player{}.
-export_type([player/0]).

players() ->
  [].

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

move(#player{target = undefined} = Player, _) ->
  Player;
move(#player{position = P, target = T, speed = S} = Player, Dt) ->
  case pathfinding:next_point(P, T, S, Dt) of
    undefined ->
      Player#player{target = undefined};
    New ->
      Player#player{position = New, update_position = true}
  end.

clean(Player) ->
  Player#player{update_position = false}.
