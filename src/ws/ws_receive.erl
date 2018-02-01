-module(ws_receive).
-author("nt").

-export([decompose_message/1, parse_click_body/1]).

decompose_message(#{<<"type">> := Type, <<"body">> := Body}) when is_bitstring(Type) ->
  {Type, Body}.

parse_click_body(#{<<"point">> := #{<<"x">> := X, <<"y">> := Y}, <<"avatar_id">> := AvatarId}) when is_number(X), is_number(Y), is_number(AvatarId) ->
  {{X, Y}, AvatarId};
parse_click_body(#{<<"point">> := #{<<"x">> := X, <<"y">> := Y}}) when is_number(X), is_number(Y) ->
  {{X, Y}, undefined}.