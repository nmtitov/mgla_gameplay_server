-module(ws_send).
-author("nt").

-export([enter/1, leave/1, teleport/2]).

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
  gproc:send(ws_handler:players_broadcast_key(), {send, M}).

enter(Id) ->
  M = jsx:encode(#{
    type => enter,
    body => #{
      id => Id
    }
  }),
  gproc:send(ws_handler:players_broadcast_key(), {send, M}).

leave(Id) ->
  M = jsx:encode(#{
    type => leave,
    body => #{
      id => Id
    }
  }),
  gproc:send(ws_handler:players_broadcast_key(), {send, M}).
