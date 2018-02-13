-module(vec).
-author("nt").

-export([is_vec/1, vec_from_points/2, magnitude/1, unit/1, add/2, subtract/2, scale/2, scalar_multiply/2]).

-type vec() :: {number(), number()}.
-export_type([vec/0]).

is_vec({X, Y}) when is_number(X), is_number(Y) -> true;
is_vec(_) -> false.

vec_from_points({X1, Y1} = A, {X2, Y2} = B) ->
  case is_vec(A) of false -> error(badarg); _ -> ok end,
  case is_vec(B) of false -> error(badarg); _ -> ok end,
  {X2 - X1, Y2 - Y1}.

magnitude({X, Y} = V) ->
  case is_vec(V) of false -> error(badarg); _ -> ok end,
  math:sqrt(X * X + Y * Y).

unit({X, Y} = V) ->
  case is_vec(V) of false -> error(badarg); _ -> ok end,
  L = magnitude(V),
  {X / L, Y / L}.

add({X1, Y1} = A, {X2, Y2} = B) ->
  case is_vec(A) of false -> error(badarg); _ -> ok end,
  case is_vec(B) of false -> error(badarg); _ -> ok end,
  {X1 + X2, Y1 + Y2}.

subtract({X1, Y1} = A, {X2, Y2} = B) ->
  case is_vec(A) of false -> error(badarg); _ -> ok end,
  case is_vec(B) of false -> error(badarg); _ -> ok end,
  {X1 - X2, Y1 - Y2}.

scale({X, Y} = V, K) ->
  case is_vec(V) of false -> error(badarg); _ -> ok end,
  {X * K, Y * K}.

scalar_multiply({X1, Y1} = A, {X2, Y2} = B) ->
  case is_vec(A) of false -> error(badarg); _ -> ok end,
  case is_vec(B) of false -> error(badarg); _ -> ok end,
  {X1 * X2, Y1 * Y2}.


%% Spec

-spec is_vec(V :: vec()) -> boolean().
-spec vec_from_points(A :: point:point(), B :: point:point()) -> vec().
-spec magnitude(V :: vec()) -> float().
-spec unit(V :: vec()) -> vec().
-spec add(A :: vec(), B :: vec()) -> vec().
-spec subtract(A :: vec(), B :: vec()) -> vec().
-spec scale(V :: vec(), K :: number()) -> vec().
-spec scalar_multiply(A :: vec(), B :: vec()) -> vec().
