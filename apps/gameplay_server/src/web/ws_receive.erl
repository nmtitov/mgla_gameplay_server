-module(ws_receive).
-author("nt").

-export([get_type_and_body/1, get_input/1]).

get_type_and_body([{<<"type">>, Type}, {<<"body">>, Body}]) when is_bitstring(Type) ->
  {Type, Body}.

get_input([{<<"y">>, Y}, {<<"x">>, X}]) when is_float(X), is_float(Y) ->
  {X, Y}.
