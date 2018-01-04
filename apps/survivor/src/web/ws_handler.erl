-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  erlang:start_timer(0, self(), init),
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  io:format("~p~n", [Msg]),
  [{<<"y">>, Y}, {<<"x">>, X}] = jsx:decode(Msg),
  Point = point:point(X, Y),
  Message = response:teleport(Point),
  {reply, {text, Message}, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _, init}, State) ->
  Message = response:teleport(point:point(0, 0)),
  {reply, {text, Message}, State};
websocket_info(_Info, State) ->
  {ok, State}.

terminate({remote, _, _}, _Req, State) ->
  io:format("Client disconnected~n"),
  ok;
terminate(Reason, _Req, _State) ->
  io:format("~p~n", [Reason]),
  ok.