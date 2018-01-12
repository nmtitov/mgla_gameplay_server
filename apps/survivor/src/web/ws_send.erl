-module(ws_send).
-author("nt").

-export([teleport/2]).

-spec teleport(id_server:id(), point:point()) -> ok.
teleport(Id, {X, Y}) ->
  M = jsx:encode(#{
    type => teleport,
    body => #{
      id => Id,
      point => #{
        x => X,
        y => Y
      }
    }
  }),
  gproc:send({p, l, {player, broadcast}}, {send, M}).
