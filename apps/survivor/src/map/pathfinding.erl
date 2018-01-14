-module(pathfinding).
-author("nt").

%% API
-export([initial_point/0, next_point/6, destination_point/3]).

-spec initial_point() -> point:point().
initial_point() ->
  point:point(100, 100).

-spec destination_point(A, B, Blocks) -> C when A :: point:point(), B :: point:point(), Blocks :: [rect:rect()], C :: point:point() | undefined.
destination_point(A, B, Blocks) ->
%%  Intersected = lists:filter(fun(Block) -> rect:intersects_line(Block, A, B) end, Blocks),
  case not lists:any(fun(Block) -> rect:intersects_line(Block, A, B) end, Blocks) of
    true -> B;
    false -> undefined
  end.

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
  Suggested = point:translate(A, Offset),
  case accessible(Suggested, MapRect, Blocks) of
    true -> validate(A, B, Suggested);
    false -> undefined
  end.

accessible(Point, MapRect, Blocks) ->
  InsideMapRect = rect:contains(MapRect, Point),
  InsideBlock = lists:any(fun(B) -> rect:contains(B, Point) end, Blocks),
  InsideMapRect and (not InsideBlock).

validate(A, B, Suggested) ->
  case point:distance(Suggested, B) < point:distance(A, B) of
    true -> Suggested;
    false -> undefined
  end.
