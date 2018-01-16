-module(point).
-author("nt").

-export([distance/2, translate/2]).

-spec distance(A, B) -> C when A :: {number(), number()}, B :: {number(), number()}, C :: float().
distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow((X2 - X1), 2) + math:pow(Y2 - Y1, 2)).

-spec translate(A, B) -> C when A :: {number(), number()}, B :: {number(), number()}, C :: {number(), number()}.
translate({X1, Y1}, {X2, Y2}) ->
  {X1 + X2, Y1 + Y2}.
