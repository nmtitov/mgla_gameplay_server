-module(path_tools).
-author("nt").

%% API
-export([initial_point/0, next_point/6, destination_point/3, do_destination_point/3]).

-spec initial_point() -> {number(), number()}.
initial_point() ->
  {100, 100}.

-spec destination_point(A, B, Blocks) -> [C] when A :: {number(), number()}, B :: {number(), number()}, Blocks :: [rect:rect()], C :: {number(), number()}.
destination_point(A, B, Blocks) ->
  Intersected = lists:filter(fun(Block) -> rect:intersects_line(Block, A, B) end, Blocks),
  do_destination_point(A, B, Intersected).

-spec do_destination_point(A, B, Intersected) -> [C] when A :: {number(), number()}, B :: {number(), number()}, Intersected :: [rect:rect()], C :: {number(), number()}.
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
  case digraph:get_short_path(G, A, B) of
    false -> [B];
    [_|Tail] -> Tail
  end.

-spec next_point(A, B, Dt, Speed, MapRect, Blocks) -> NextPoint when
  A :: {number(), number()},
  B :: {number(), number()},
  Dt :: float(),
  Speed :: float(),
  MapRect :: rect:rect(),
  Blocks :: [rect:rect()],
  NextPoint :: {number(), number()} | undefined.
next_point(A, B, Dt, Speed, MapRect, Blocks) ->
  Distance = Dt * Speed,
  Unit = vec:unit(vec:vec_from_points(A, B)),
  Offset = vec:scale(Unit, Distance),
  Suggested = point_tools:translate(A, Offset),
  case accessible(Suggested, MapRect, Blocks) of
    true -> validate(A, B, Suggested);
    false -> undefined
  end.

accessible(Point, MapRect, Blocks) ->
  InsideMapRect = rect:contains(MapRect, Point),
  InsideBlock = lists:any(fun(B) -> rect:contains(B, Point) end, Blocks),
  InsideMapRect and (not InsideBlock).

validate(A, B, Suggested) ->
  case point_tools:distance(Suggested, B) < point_tools:distance(A, B) of
    true -> Suggested;
    false -> undefined
  end.
