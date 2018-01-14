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

%% API
-export([rect/2, contains/2, intersects_line/3, vertices/1, wrapper/1]).

-type rect() :: {{float(), float()}, {float(), float()}}.
-export_type([rect/0]).

-spec rect(Origin, Size) -> Rect when Origin :: point:point(), Size :: size:size(), Rect :: rect().
rect(Origin, Size) -> {Origin, Size}.

-spec contains(Rect, Point) -> boolean() when Rect :: rect(), Point :: point:point().
contains({{OriginX, OriginY}, {W, H}}, {X, Y}) -> ((OriginX < X) and (X < (OriginX + W))) and ((OriginY < Y) and (Y < (OriginY + H))).

intersects_line({{OriginX, OriginY}, {W, H}}, {X1, Y1} = A, {X2, Y2} = B) ->
  RectMaxX = OriginX + W,
  RectMaxY = OriginY + H,
  {XMin, XMax} = asc(X1, X2),
  {YMin, YMax} = asc(Y1, Y2),
  if
    (OriginX > XMax) or (RectMaxX < XMin) -> false;
    true ->
      if
        (OriginY > YMax) or (RectMaxY < YMin) -> false;
        true ->
          YAtRectOrigin = calculate_y_for_x(OriginX, A, B),
          YAtRectMax = calculate_y_for_x(RectMaxX, A, B),
          if
            (OriginY > YAtRectOrigin) and (OriginY > YAtRectMax) -> false;
            true ->
              if
                (RectMaxY < YAtRectOrigin) and (RectMaxY < YAtRectMax) -> false;
                true -> true
              end
          end
      end
  end.

-spec calculate_y_for_x(X, A, B) -> Y when X :: float(), A :: point:point(), B :: point:point(), Y :: float().
calculate_y_for_x(_, {X1, _}, {X1, _}) ->
  error(badarg);
calculate_y_for_x(X, {X1, Y1}, {X2, Y2}) ->
  ((X - X1) / (X2 - X1)) * (Y2 - Y1) + Y1.

asc(X, Y) when X < Y -> {X, Y};
asc(X, Y) -> {Y, X}.

-spec vertices(R) -> [V] when R :: rect(), V :: point:point().
vertices({{OriginX, OriginY}, {W, H}}) ->
  [
    point:point(OriginX, OriginY),
    point:point(OriginX + W, OriginY),
    point:point(OriginX + W, OriginY + H),
    point:point(OriginX, OriginY + H)
  ].

-spec wrapper(R1) -> R2 when R1 :: rect(), R2 :: rect().
wrapper({{OriginX, OriginY}, {W, H}}) ->
  rect(point:point(OriginX - 1, OriginY - 1), size:size(W + 2, H + 2)).
