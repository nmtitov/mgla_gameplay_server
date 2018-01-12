-module(ws_send).
-author("nt").

-export([teleport/2]).

-spec teleport(non_neg_integer(), point:point()) -> ok.
teleport(Id, {X, Y}) ->
  M = jsx:encode(#{
    id => Id,
    point => #{
      x => X,
      y => Y
    }
  }),
  gproc:send({p, l, {player, broadcast}}, {send, M}).
