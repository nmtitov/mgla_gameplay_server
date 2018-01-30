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
-include("../../include/block.hrl").

%% API
-export([is_rect/1, contains/2, intersects_line/3, vertices/1, visible_vertices/2]).

-type rect() :: {point:point(), point:point()}.
-export_type([rect/0]).

is_rect({{OriginX, OriginY}, {W, H}}) when is_number(OriginX), is_number(OriginY), is_number(W), is_number(H) -> true;
is_rect(_) -> false.

-spec contains(Rect, Point) -> boolean() when Rect :: rect(), Point :: point:point().
contains({{OriginX, OriginY}, {W, H}} = R, {X, Y} = P) ->
  case is_rect(R) of false -> error(badarg); _ -> ok end,
  case point:is_point(P) of false -> error(badarg); _ -> ok end,
  ((OriginX < X) and (X < (OriginX + W))) and ((OriginY < Y) and (Y < (OriginY + H))).

intersects_line({{OriginX, OriginY}, {W, H}} = R, {X1, Y1} = A, {X2, Y2} = B) ->
  case is_rect(R) of false -> error(badarg); _ -> ok end,
  case point:is_point(A) of false -> error(badarg); _ -> ok end,
  case point:is_point(B) of false -> error(badarg); _ -> ok end,
  RectMaxX = OriginX + W,
  RectMaxY = OriginY + H,
  [LineXMin, LineXMax] = lists:sort([X1, X2]),
  [LineYMin, LineYMax] = lists:sort([Y1, Y2]),
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

-spec calculate_y_for_x(X :: number(), A :: point:point(), B :: point:point()) -> float().
calculate_y_for_x(_, {X1, _}, {X1, _}) ->
  error(badarg);
calculate_y_for_x(X, {X1, Y1} = A, {X2, Y2} = B) when is_number(X) ->
  case point:is_point(A) of false -> error(badarg); _ -> ok end,
  case point:is_point(B) of false -> error(badarg); _ -> ok end,
  ((X - X1) / (X2 - X1)) * (Y2 - Y1) + Y1.

-spec vertices(R :: rect()) -> [point:point()].
vertices({{OriginX, OriginY}, {W, H}} = R) ->
  case is_rect(R) of false -> error(badarg); _ -> ok end,
  [
    {OriginX, OriginY},
    {OriginX + W, OriginY},
    {OriginX + W, OriginY + H},
    {OriginX, OriginY + H}
  ].

-spec visible_vertices(Block, A) -> [V] when Block :: block(), A :: point:point(), V :: point:point().
visible_vertices(#block{rect = R, graph_vertices = Vs}, A) ->
  lists:filter(fun(V) -> not intersects_line(R, A, V) end, Vs).
