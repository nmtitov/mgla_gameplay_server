-module(websocket_handler).

-export([teleport/2]).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(state, {

}).

-spec teleport(pid(), point:point()) -> ok.
teleport(WebSocket, P) ->
  WebSocket ! {teleport, P}, ok.

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(_) ->
  world_server:connect(self()),
  {ok, #state{}}.

websocket_handle({text, Msg}, State) ->
  io:format("~p~n", [Msg]),
  [{<<"y">>, Y}, {<<"x">>, X}] = jsx:decode(Msg),
  P = point:point(X, Y),
  world_server:target(self(), P),
  {ok, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({teleport, P}, State) ->
  Message = response:teleport(P),
  {reply, {text, Message}, State};
websocket_info(_Info, State) ->
  {ok, State}.


terminate({error, closed}, _Req, _) ->
  io:format("Client disconnected~n"),
  world_server:disconnect(self()),
  ok;
terminate(Reason, _Req, _State) ->
  io:format("~p~n", [Reason]),
  ok.
