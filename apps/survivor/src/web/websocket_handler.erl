-module(websocket_handler).

-export([teleport/2]).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(state, {
  id :: non_neg_integer()
}).

-spec teleport(non_neg_integer(), point:point()) -> ok.
teleport(Id, P) ->
  gproc:send({p, l, {player, broadcast}}, {teleport, Id, P}).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(_) ->
  Id = id_server:id(),
  gproc:reg({n, l, {player, Id}}),
  gproc:reg({p, l, {player, broadcast}}),
  world_server:enter(Id),
  {ok, #state{id = Id}}.

websocket_handle({text, Msg}, #state{id = Id} = State) ->
  io:format("~p~n", [Msg]),
  [{<<"y">>, Y}, {<<"x">>, X}] = jsx:decode(Msg),
  P = point:point(X, Y),
  world_server:input(Id, P),
  {ok, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({teleport, Id, P}, State) ->
  Message = response:teleport(Id, P),
  {reply, {text, Message}, State};
websocket_info(_Info, State) ->
  {ok, State}.

terminate({error, closed}, _Req, #state{id = Id}) ->
  io:format("Client disconnected~n"),
  gproc:unreg({n, l, {player, Id}}),
  gproc:unreg({p, l, {player, broadcast}}),
  world_server:leave(Id),
  ok;
terminate(Reason, _Req, _State) ->
  io:format("~p~n", [Reason]),
  ok.
