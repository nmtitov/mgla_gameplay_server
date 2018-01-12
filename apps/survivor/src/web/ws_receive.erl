-module(ws_receive).
-author("nt").

-export([decode/1, input/1]).

decode(Message) ->
  io:format("~p~n", [Message]),
  [{<<"type">>, Type}, {<<"body">>, Body}] = jsx:decode(Message),
  {Type, Body}.

input([{<<"y">>, Y}, {<<"x">>, X}]) ->
  point:point(X, Y).
