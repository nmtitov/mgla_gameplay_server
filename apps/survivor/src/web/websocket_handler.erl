-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-define(TICK, tick).
-define(TICK_RATE, 33).

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
  Reply = if
    is_float(TargetX) and is_float(TargetY) ->
      Current = point:point(X, Y),
      Target = point:point(TargetX, TargetY),
      case new_point(Current, Target, ?TICK_RATE / 1000.0, 100.0) of
        undefined ->
          {ok, State#state{target_x = undefined, target_y = undefined}};
        NewPoint  ->
          Message = response:teleport(NewPoint),
          {NewX, NewY} = NewPoint,
          {reply, {text, Message}, State#state{x = NewX, y = NewY}}
      end;
    true ->
      {ok, State}
  end,
  schedule_next_tick(),
  Reply;

websocket_info(_Info, State) ->
  {ok, State}.


terminate({remote, _, _}, _Req, _) ->
  io:format("Client disconnected~n"),
  ok;

terminate(Reason, _Req, _State) ->
  io:format("~p~n", [Reason]),
  ok.

%%

schedule_next_tick() -> erlang:start_timer(?TICK_RATE, self(), ?TICK).

-spec new_point(point:point(), point:point(), float(), float()) -> point:point() | undefined.
new_point(Current, Target, Dt, Speed) ->
  Distance = Dt * Speed,
  Unit = vec:unit(vec:vec_from_points(Current, Target)),
  Offset = vec:scale(Unit, Distance),
  NewPoint = point:translate(Current, Offset),
  CurrentDistanceToTarget = point:distance(Current, Target),
  NewDistanceToTarget = point:distance(NewPoint, Target),
  if
    NewDistanceToTarget < CurrentDistanceToTarget ->
      NewPoint;
    true ->
      undefined
  end.