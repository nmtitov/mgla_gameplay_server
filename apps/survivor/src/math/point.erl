-module(point).
-author("nt").

-export([point/2, x/1, y/1]).

-type point() :: {float(), float()}.
-export_type([point/0]).

-spec point(float(), float()) -> point().
point(X, Y) -> {float(X), float(Y)}.

-spec x(point()) -> float().
x({X, _}) -> X.

-spec y(point()) -> float().
y({_, Y}) -> Y.
