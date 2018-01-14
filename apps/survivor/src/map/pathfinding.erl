-module(pathfinding).
-author("nt").

%% API
-export([initial_point/0, next_point/6, destination_point/3]).

-spec initial_point() -> point:point().
initial_point() ->
  point:point(100, 100).

-spec destination_point(A, B, Blocks) -> [C] when A :: point:point(), B :: point:point(), Blocks :: [rect:rect()], C :: point:point().
destination_point(A, B, Blocks) ->
  Intersected = lists:filter(fun(Block) -> rect:intersects_line(Block, A, B) end, Blocks),
  do_destination_point(A, B, Intersected).

-spec do_destination_point(A, B, Intersected) -> [C] when A :: point:point(), B :: point:point(), Intersected :: [rect:rect()], C :: point:point().
do_destination_point(_, B, []) -> [B];
do_destination_point(A, B, [Block|_]) ->
  Vertices = rect:vertices(Block),
  VisibleFromA = rect:visible_vertices(Block, A),
  VisibleFromB = rect:visible_vertices(Block, B),
  G = digraph:new(),
  digraph:add_vertex(G, A),
  digraph:add_vertex(G, B),
  lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, Vertices),
  lists:foreach(fun(V) -> digraph:add_edge(G, A, V) end, VisibleFromA),
  lists:foreach(fun(V) -> digraph:add_edge(G, V, B) end, VisibleFromB),
  [VH|VT] = Vertices,
  InternalEdges = lists:zip(Vertices, VT ++ [VH]),
  lists:foreach(fun({From, To}) -> digraph:add_edge(G, From, To), digraph:add_edge(G, To, From) end, InternalEdges),
  Path = digraph:get_short_path(G, A, B),
  [_|Tail] = Path,
  Tail.

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
