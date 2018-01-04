-module(vec).
-author("nt").

-export([vec/2, x/1, y/1, magnitude/1, unit/1, add/2, subtract/2, scale/2, scalar_multiply/2]).

-type vec() :: {float(), float()}.
-export_type([vec/0]).

-spec vec(number(), number()) -> vec().
vec(X, Y) ->
  {float(X), float(Y)}.

-spec x(vec()) -> float().
x({X, _}) ->
  X.

-spec y(vec()) -> float().
y({_, Y}) ->
  Y.

-spec magnitude(vec()) -> float().
magnitude({X, Y}) ->
  math:sqrt(X * X + Y * Y).

-spec unit(vec()) -> vec().
unit({X, Y} = V) ->
  L = magnitude(V),
  vec(X / L, Y / L).

-spec add(vec(), vec()) -> vec().
add({X, Y}, {X1, Y1}) ->
  vec(X + X1, Y + Y1).

-spec subtract(vec(), vec()) -> vec().
subtract({X, Y}, {X1, Y1}) ->
  vec(X - X1, Y - Y1).

-spec scale(vec(), float()) -> vec().
scale({X, Y}, K) ->
  vec(X * K, Y * K).

-spec scalar_multiply(vec(), vec()) -> vec().
scalar_multiply({X, Y}, {X1, Y1}) ->
  vec(X * X1, Y * Y1).
