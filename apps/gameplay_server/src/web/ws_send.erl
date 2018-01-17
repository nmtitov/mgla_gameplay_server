-module(ws_send).
-author("nt").
-include("../map/header/player_state.hrl").
-export([broadcast_enter/1, send_map/1, broadcast_leave/1, broadcast_update/3]).

-spec broadcast_update(Id, Point, State) -> ok when Id :: id_server:id(), Point :: point:point(), State :: player_state().
broadcast_update(Id, {X, Y}, State) ->
  M = jsx:encode(#{
    type => teleport,
    body => #{
      id => Id,
      point => #{
        x => X,
        y => Y
      },
      new_state => if
                 State == undefined -> null;
                 true -> State
      end
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
