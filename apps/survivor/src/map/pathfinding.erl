-module(pathfinding).
-author("nt").

%% API
-export([initial_point/0, next_point/5]).

-spec initial_point() -> point:point().
initial_point() ->
  point:point(0, 0).

-spec next_point(A, B, Dt, Speed, MapSize) -> NextPoint when
  A :: point:point(),
  B :: point:point(),
  Dt :: float(),
  Speed :: float(),
  MapSize :: size:size(),
  NextPoint :: point:point() | undefined.
next_point(A, B, Dt, Speed, MapSize) ->
  Distance = Dt * Speed,
  Unit = vec:unit(vec:vec_from_points(A, B)),
  Offset = vec:scale(Unit, Distance),
  NextPoint = point:translate(A, Offset),
  case point_inside(NextPoint, MapSize) of
    true ->
      CurrentDistanceToTarget = point:distance(A, B),
      NewDistanceToTarget = point:distance(NextPoint, B),
      if
        NewDistanceToTarget < CurrentDistanceToTarget ->
          NextPoint;
        true ->
          undefined
      end;
    false ->
      undefined
  end.

point_inside({X, Y}, {W, H}) ->
  ((0 < X) and (X < W)) and ((0 < Y) and (Y < H)).
