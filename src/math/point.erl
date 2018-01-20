-module(point).
-author("nt").

-export([distance/2, translate/2]).

-type point() :: {number(), number()}.
-export_type([point/0]).

-spec distance(A, B) -> C when A :: point(), B :: point(), C :: float().
distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow((X2 - X1), 2) + math:pow(Y2 - Y1, 2)).

-spec translate(A, B) -> C when A :: point(), B :: point(), C :: point().
translate({X1, Y1}, {X2, Y2}) ->
  {X1 + X2, Y1 + Y2}.
