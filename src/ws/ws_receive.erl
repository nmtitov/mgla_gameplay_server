-module(ws_receive).
-author("nt").

-export([decompose_message/1, extract_point/1]).

decompose_message([{<<"type">>, Type}, {<<"body">>, Body}]) when is_bitstring(Type) ->
  {Type, Body}.

extract_point([{<<"y">>, Y}, {<<"x">>, X}]) when is_number(X), is_number(Y) ->
  {X, Y}.
