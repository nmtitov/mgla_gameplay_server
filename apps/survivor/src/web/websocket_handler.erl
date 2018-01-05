-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-define(TICK, tick).

-record(state, {
  id = 1,
  x = undefined,
  y = undefined
  }).


init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.


websocket_init(State) ->
  erlang:start_timer(0, self(), init),
  {ok, #state{}}.


websocket_handle({text, Msg}, State) ->
  io:format("~p~n", [Msg]),
  [{<<"y">>, Y}, {<<"x">>, X}] = jsx:decode(Msg),
  {ok, State#state{x = X, y = Y}};

websocket_handle(_Data, State) ->
  {ok, State}.


websocket_info({timeout, _, init}, State) ->
  Message = response:teleport(point:point(0, 0)),
  schedule_next_tick(),
  {reply, {text, Message}, State};

websocket_info({timeout, _, ?TICK}, State=#state{x = X, y = Y}) ->
  if
    is_float(X) and is_float(Y) ->
      io:format("X, Y are integer~n"),
      Point = point:point(X, Y),
      Message = response:teleport(Point),
      schedule_next_tick(),
      {reply, {text, Message}, State#state{x = undefined, y = undefined}};
    true ->
      io:format("X, Y are undefined~n"),
      schedule_next_tick(),
      {ok, State}
  end;

websocket_info(_Info, State) ->
  {ok, State}.


terminate({remote, _, _}, _Req, State) ->
  io:format("Client disconnected~n"),
  ok;

terminate(Reason, _Req, _State) ->
  io:format("~p~n", [Reason]),
  ok.

%%

schedule_next_tick() -> erlang:start_timer(33, self(), ?TICK).