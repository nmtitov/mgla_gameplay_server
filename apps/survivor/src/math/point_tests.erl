-module(point_tests).
-author("nt").

-include_lib("eunit/include/eunit.hrl").


create_test() ->
  X = 1.0,
  Y = 1.0,
  P = point:point(X, Y),
  X = point:x(P),
  Y = point:y(P),
  {X, Y} = P.
