-module(point).
-author("nt").

-export([point/2, x/1, y/1, distance/2, translate/2]).

-type point() :: {float(), float()}.
-export_type([point/0]).

-spec point(number(), number()) -> point().
point(X, Y) -> {float(X), float(Y)}.

-spec x(point()) -> float().
x({X, _}) -> X.

-spec y(point()) -> float().
y({_, Y}) -> Y.

-spec distance(point(), point()) -> float().
distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow((X2 - X1), 2) + math:pow(Y2 - Y1, 2)).

-spec translate(point(), vec:vec()) -> point().
translate({X1, Y1}, {X2, Y2}) ->
  point(X1 + X2, Y1 + Y2).
