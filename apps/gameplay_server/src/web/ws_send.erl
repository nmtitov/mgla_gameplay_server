-module(ws_send).
-author("nt").

-export([broadcast_enter/1, send_map/1, broadcast_leave/1, broadcast_update/2]).

-spec broadcast_update(id_server:id(), point:point()) -> ok.
broadcast_update(Id, {X, Y}) ->
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

broadcast_enter(Id) ->
  M = jsx:encode(#{
    type => enter,
    body => #{
      id => Id
    }
  }),
  gproc:send(ws_handler:players_broadcast_key(), {send, M}).

send_map(Id) ->
  gproc:send(ws_handler:players_key(Id), {send, map_tools:map()}).

broadcast_leave(Id) ->
  M = jsx:encode(#{
    type => leave,
    body => #{
      id => Id
    }
  }),
  gproc:send(ws_handler:players_broadcast_key(), {send, M}).
