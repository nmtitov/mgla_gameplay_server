-module(movement).
-author("nt").

%% API
-export([initial_point/0, next_point/4]).

-spec initial_point() -> point:point().
initial_point() ->
  point:point(0, 0).

-spec next_point(point:point(), point:point(), float(), float()) -> point:point() | undefined.
next_point(Current, Target, Dt, Speed) ->
  Distance = Dt * Speed,
  Unit = vec:unit(vec:vec_from_points(Current, Target)),
  Offset = vec:scale(Unit, Distance),
  NewPoint = point:translate(Current, Offset),
  CurrentDistanceToTarget = point:distance(Current, Target),
  NewDistanceToTarget = point:distance(NewPoint, Target),
  if
    NewDistanceToTarget < CurrentDistanceToTarget ->
      NewPoint;
    true ->
      undefined
  end.