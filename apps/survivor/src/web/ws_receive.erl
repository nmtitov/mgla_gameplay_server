-module(ws_receive).
-author("nt").

-export([input/1]).

input(M) ->
  io:format("~p~n", [M]),
  [{<<"y">>, Y}, {<<"x">>, X}] = jsx:decode(M),
  P = point:point(X, Y),
  {ok, P}.