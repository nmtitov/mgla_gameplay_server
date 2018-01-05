-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-define(TICK, tick).

-record(state, {
  id = 1,
  x = undefined,
  y = undefined,
  target_x = undefined,
  target_y = undefined
  }).


init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.


websocket_init(_) ->
  erlang:start_timer(0, self(), init),
  {ok, #state{}}.


websocket_handle({text, Msg}, State) ->
  io:format("~p~n", [Msg]),
  [{<<"y">>, Y}, {<<"x">>, X}] = jsx:decode(Msg),
  {ok, State#state{target_x = X, target_y = Y}};

websocket_handle(_Data, State) ->
  {ok, State}.


websocket_info({timeout, _, init}, State) ->
  X = 0,
  Y = 0,
  Message = response:teleport(point:point(X, Y)),
  schedule_next_tick(),
  {reply, {text, Message}, State#state{x = X, y = Y}};

websocket_info({timeout, _, ?TICK}, State=#state{x = X, y = Y, target_x = TargetX, target_y = TargetY}) ->
  if
    is_float(TargetX) and is_float(TargetY) ->
      io:format("true~n"),
      Vec = vec:vec(TargetX - X, TargetY - Y),
      {X1, Y1} = vec:scale(Vec, 0.01),
      NewX = X + X1,
      NewY = Y + Y1,
      Point = point:point(NewX, NewY),
      Message = response:teleport(Point),
      schedule_next_tick(),
      {reply, {text, Message}, State#state{x = NewX, y = NewY}};
    true ->
      io:format("false~n"),
      schedule_next_tick(),
      {ok, State}
  end;

websocket_info(_Info, State) ->
  {ok, State}.


terminate({remote, _, _}, _Req, _) ->
  io:format("Client disconnected~n"),
  ok;

terminate(Reason, _Req, _State) ->
  io:format("~p~n", [Reason]),
  ok.

%%

schedule_next_tick() -> erlang:start_timer(33, self(), ?TICK).