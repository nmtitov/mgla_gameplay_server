-module(point).
-author("nt").

-export([is_point/1, distance/2, translate/2, pointToMap/1]).

-type point() :: {number(), number()}.
-export_type([point/0]).

is_point({X, Y}) when is_number(X), is_number(Y) -> true;
is_point(_) -> false.

distance({X1, Y1} = A, {X2, Y2} = B) ->
  case is_point(A) of false -> error(badarg); _ -> ok end,
  case is_point(B) of false -> error(badarg); _ -> ok end,
  math:sqrt(math:pow((X2 - X1), 2) + math:pow(Y2 - Y1, 2)).

translate({X1, Y1} = A, {X2, Y2} = B) ->
  case is_point(A) of false -> error(badarg); _ -> ok end,
  case is_point(B) of false -> error(badarg); _ -> ok end,
  {X1 + X2, Y1 + Y2}.

pointToMap({X, Y} = A) ->
  case is_point(A) of false -> error(badarg); _ -> ok end,
  #{
    x => X,
    y => Y
  }.


%% Spec
-spec is_point(P :: point()) -> boolean().
-spec distance(A :: point(), B :: point()) -> float().
-spec translate(A :: point(), B :: point()) -> point().
-spec pointToMap(A :: point()) -> #{x:= number(), y := number()}.
