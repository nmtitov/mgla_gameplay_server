-module(ws_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(state, {
  id :: non_neg_integer()
}).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(_) ->
  Id = id_server:id(),
  gproc:reg({n, l, {player, Id}}),
  gproc:reg({p, l, {player, broadcast}}),
  map_server:enter(Id),
  {ok, #state{id = Id}}.

websocket_handle({text, M}, #state{id = Id} = State) ->
  {ok, P} = ws_receive:input(M),
  map_server:input(Id, P),
  {ok, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({send, Message}, State) ->
  {reply, {text, Message}, State};
websocket_info(_Info, State) ->
  {ok, State}.

terminate({error, closed}, _Req, #state{id = Id}) ->
  io:format("Client disconnected~n"),
  gproc:unreg({n, l, {player, Id}}),
  gproc:unreg({p, l, {player, broadcast}}),
  map_server:leave(Id),
  ok;
terminate(Reason, _Req, _State) ->
  io:format("~p~n", [Reason]),
  ok.
