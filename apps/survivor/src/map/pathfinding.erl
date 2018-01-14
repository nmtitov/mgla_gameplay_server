-module(pathfinding).
-author("nt").

%% API
-export([initial_point/0, next_point/6]).

-spec initial_point() -> point:point().
initial_point() ->
  point:point(0, 0).

-spec next_point(A, B, Dt, Speed, MapRect, Blocks) -> NextPoint when
  A :: point:point(),
  B :: point:point(),
  Dt :: float(),
  Speed :: float(),
  MapRect :: rect:rect(),
  Blocks :: [rect:rect()],
  NextPoint :: point:point() | undefined.
next_point(A, B, Dt, Speed, MapRect, Blocks) ->
  Distance = Dt * Speed,
  Unit = vec:unit(vec:vec_from_points(A, B)),
  Offset = vec:scale(Unit, Distance),
  NextPoint = point:translate(A, Offset),
  case accessible(NextPoint, MapRect, Blocks) of
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

accessible(Point, MapRect, Blocks) ->
  InsideMapRect = rect:contains(MapRect, Point),
  InsideBlock = lists:any(fun(B) -> rect:contains(B, Point) end, Blocks),
  InsideMapRect and (not InsideBlock).
