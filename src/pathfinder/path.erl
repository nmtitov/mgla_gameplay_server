-module(path).
-author("nt").
-include("../../include/block.hrl").

%% API
-export([initial_point/2, next_point/6, path/3]).

-spec initial_point(R, Blocks) -> P when R :: rect:rect(), Blocks :: [rect:rect()], P :: point:point().
initial_point({{X, Y}, {W, H}} = Rect, Blocks) ->
  P = {rand:uniform(W) + X, rand:uniform(H) + Y},
  Out = not lists:any(fun(#block{rect = BlockRect}) -> rect:contains(BlockRect, P) end, Blocks),
  case Out of
    true -> P;
    _    -> initial_point(Rect, Blocks)
  end.

-spec path(A, B, Blocks) -> [C] when A :: point:point(), B :: point:point(), Blocks :: [rect:rect()], C :: point:point().
path(A, B, Blocks) ->
  Intersected = lists:filter(fun(#block{rect = R}) -> rect:intersects_line(R, A, B) end, Blocks),
  do_destination_point(A, B, Intersected).

-spec do_destination_point(A, B, Intersected) -> [C] when A :: point:point(), B :: point:point(), Intersected :: [rect:rect()], C :: point:point().
do_destination_point(_, B, []) -> [B];
do_destination_point(A, B, [Block|_]) ->
  Vertices = Block#block.graph_vertices,
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

-spec next_point(A, B, Dt, Speed, InRect, Blocks) -> NextPoint when
  A :: point:point(),
  B :: point:point(),
  Dt :: float(),
  Speed :: float(),
  InRect :: rect:rect(),
  Blocks :: [rect:rect()],
  NextPoint :: point:point() | undefined.
next_point(A, B, Dt, Speed, InRect, Blocks) ->
  Distance = Dt * Speed,
  Unit = vec:unit(vec:vec_from_points(A, B)),
  Offset = vec:scale(Unit, Distance),
  Suggested = point:translate(A, Offset),
  case accessible(Suggested, InRect, Blocks) of
    true -> validate(A, B, Suggested);
    false -> undefined
  end.

accessible(Point, MapRect, Blocks) ->
  InsideMapRect = rect:contains(MapRect, Point),
  InsideBlock = lists:any(fun(#block{rect = R}) -> rect:contains(R, Point) end, Blocks),
  InsideMapRect and (not InsideBlock).

validate(A, B, Suggested) ->
  case point:distance(Suggested, B) < point:distance(A, B) of
    true -> Suggested;
    false -> undefined
  end.
