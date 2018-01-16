%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright © 2018 N. M. Titov. All rights reserved.
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2018 13:27
%%%-------------------------------------------------------------------
-module(rect).
-author("nt").
-include("../map/block.hrl").

%% API
-export([contains/2, intersects_line/3, vertices/1, visible_vertices/2]).

-type rect() :: {{number(), number()}, {number(), number()}}.
-export_type([rect/0]).

-spec contains(Rect, Point) -> boolean() when Rect :: rect(), Point :: {number(), number()}.
contains({{OriginX, OriginY}, {W, H}}, {X, Y}) -> ((OriginX < X) and (X < (OriginX + W))) and ((OriginY < Y) and (Y < (OriginY + H))).

intersects_line({{OriginX, OriginY}, {W, H}}, {X1, Y1} = A, {X2, Y2} = B) ->
  RectMaxX = OriginX + W,
  RectMaxY = OriginY + H,
  {LineXMin, LineXMax} = asc(X1, X2),
  {LineYMin, LineYMax} = asc(Y1, Y2),
  if
    (OriginX > LineXMax) or (RectMaxX < LineXMin) or (OriginY > LineYMax) or (RectMaxY < LineYMin) -> false;
    true ->
      LineYAtRectOriginX = calculate_y_for_x(OriginX, A, B),
      LineYAtRectMaxX = calculate_y_for_x(RectMaxX, A, B),
      if
        ((OriginY > LineYAtRectOriginX) and (OriginY > LineYAtRectMaxX)) or ((RectMaxY < LineYAtRectOriginX) and (RectMaxY < LineYAtRectMaxX)) -> false;
        true -> true
      end
  end.

%% P0 = {0, 0). S = {10, 10). R = rect:rect(P0, S). PX1 = {0, -5). PX2 = {0, 0). P1 = {5, -5). P2 = {5, 0). A = {-20, -20). B = {20, 20). In = {5,5). Blocks = [R].

-spec calculate_y_for_x(X, A, B) -> Y when X :: float(), A :: {number(), number()}, B :: {number(), number()}, Y :: float().
calculate_y_for_x(_, {X1, _}, {X1, _}) ->
  error(badarg);
calculate_y_for_x(X, {X1, Y1}, {X2, Y2}) ->
  ((X - X1) / (X2 - X1)) * (Y2 - Y1) + Y1.

asc(X, Y) when X < Y -> {X, Y};
asc(X, Y) -> {Y, X}.

-spec vertices(R) -> [V] when R :: rect(), V :: {number(), number()}.
vertices({{OriginX, OriginY}, {W, H}}) ->
  [
    {OriginX, OriginY},
    {OriginX + W, OriginY},
    {OriginX + W, OriginY + H},
    {OriginX, OriginY + H}
  ].

-spec visible_vertices(Block, A) -> [V] when Block :: block(), A :: {number(), number()}, V :: {number(), number()}.
visible_vertices(#block{rect = R, graph_vertices = Vs}, A) ->
  lists:filter(fun(V) -> not intersects_line(R, A, V) end, Vs).
