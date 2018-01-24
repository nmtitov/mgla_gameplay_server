-module(ws_send).
-author("nt").
-include("../../include/avatar_state.hrl").
-export([send_update/4, broadcast_update/3, send_enter/2, broadcast_enter/1, send_map/1, broadcast_leave/1]).

-spec broadcast_update(Id, Point, State) -> ok when Id :: id_server:id(), Point :: point:point(), State :: avatar_state().
broadcast_update(Id, Point, State) ->
  Message = update_message(Id, Point, State),
  gproc:send(ws_handler:broadcast_property(), {send, Message}).

send_update(ToId, Id, Point, State) ->
  Message = update_message(Id, Point, State),
  gproc:send(ws_handler:name(ToId), {send, Message}).

-spec update_message(Id, Point, State) -> Message when Id :: id_server:id(), Point :: point:point(), State :: avatar_state(), Message :: map().
update_message(Id, {X, Y}, State) ->
  jsx:encode(#{
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
  }).

send_enter(ToId, Id) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  Message = enter_message(Id),
  gproc:send(ws_handler:name(ToId), {send, Message}).

broadcast_enter(Id) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  Message = enter_message(Id),
  gproc:send(ws_handler:broadcast_property(), {send, Message}).

enter_message(Id) ->
  jsx:encode(#{
    type => enter,
    body => #{
      id => Id
    }
  }).

send_map(Id) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  gproc:send(ws_handler:name(Id), {send, map_tools:map()}).

broadcast_leave(Id) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  M = jsx:encode(#{
    type => leave,
    body => #{
      id => Id
    }
  }),
  gproc:send(ws_handler:broadcast_property(), {send, M}).
