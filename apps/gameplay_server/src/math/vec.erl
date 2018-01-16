-module(vec).
-author("nt").

-export([vec_from_points/2, magnitude/1, unit/1, add/2, subtract/2, scale/2, scalar_multiply/2]).

-spec vec_from_points(A, B) -> C when A :: {number(), number()}, B :: {number(), number()}, C :: {number(), number()}.
vec_from_points({X1, Y1}, {X2, Y2}) ->
  {X2 - X1, Y2 - Y1}.

-spec magnitude({number(), number()}) -> float().
magnitude({X, Y}) ->
  math:sqrt(X * X + Y * Y).

-spec unit({number(), number()}) -> {float(), float()}.
unit({X, Y} = V) ->
  L = magnitude(V),
  {X / L, Y / L}.

-spec add({number(), number()}, {number(), number()}) -> {number(), number()}.
add({X1, Y1}, {X2, Y2}) ->
  {X1 + X2, Y1 + Y2}.

-spec subtract({number(), number()}, {number(), number()}) -> {number(), number()}.
subtract({X1, Y1}, {X2, Y2}) ->
  {X1 - X2, Y1 - Y2}.

-spec scale({number(), number()}, number()) -> {number(), number()}.
scale({X, Y}, K) ->
  {X * K, Y * K}.

-spec scalar_multiply({number(), number()}, {number(), number()}) -> {number(), number()}.
scalar_multiply({X1, Y1}, {X2, Y2}) ->
  {X1 * X2, Y1 * Y2}.
