-module(vec).
-author("nt").

-export([vec_from_points/2, magnitude/1, unit/1, add/2, subtract/2, scale/2, scalar_multiply/2]).

-type vec() :: {number(), number()}.
-export_type([vec/0]).

-spec vec_from_points(A, B) -> C when A :: point:point(), B :: point:point(), C :: vec:vec().
vec_from_points({X1, Y1}, {X2, Y2}) ->
  {X2 - X1, Y2 - Y1}.

-spec magnitude(vec:vec()) -> float().
magnitude({X, Y}) ->
  math:sqrt(X * X + Y * Y).

-spec unit(vec:vec()) -> {float(), float()}.
unit({X, Y} = V) ->
  L = magnitude(V),
  {X / L, Y / L}.

-spec add(vec:vec(), vec:vec()) -> vec:vec().
add({X1, Y1}, {X2, Y2}) ->
  {X1 + X2, Y1 + Y2}.

-spec subtract(vec:vec(), vec:vec()) -> vec:vec().
subtract({X1, Y1}, {X2, Y2}) ->
  {X1 - X2, Y1 - Y2}.

-spec scale(vec:vec(), number()) -> vec:vec().
scale({X, Y}, K) ->
  {X * K, Y * K}.

-spec scalar_multiply(vec:vec(), vec:vec()) -> vec:vec().
scalar_multiply({X1, Y1}, {X2, Y2}) ->
  {X1 * X2, Y1 * Y2}.
