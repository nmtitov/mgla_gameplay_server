-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-define(TICK_RATE, 33).

-record(state, {
  id = 1,
  current = undefined,
  target = undefined
  }).


init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.


websocket_init(_) ->
  erlang:start_timer(0, self(), init),
  {ok, #state{}}.


websocket_handle({text, Msg}, State) ->
  io:format("~p~n", [Msg]),
  [{<<"y">>, Y}, {<<"x">>, X}] = jsx:decode(Msg),
  P = point:point(X, Y),
  {ok, State#state{target = P}};

websocket_handle(_Data, State) ->
  {ok, State}.


websocket_info({timeout, _, init}, State) ->
  P = movement:initial_point(),
  Message = response:teleport(P),
  schedule_next_tick(),
  {reply, {text, Message}, State#state{current = P}};

websocket_info({timeout, _, tick}, State=#state{current = C, target = T}) ->
  Reply = case T of
     undefined ->
       {ok, State};
     T ->
       case movement:next_point(C, T, ?TICK_RATE / 1000.0, 100.0) of
         undefined ->
           {ok, State#state{target = undefined}};
         New ->
           Message = response:teleport(New),
           {reply, {text, Message}, State#state{current = New}}
       end
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

schedule_next_tick() ->
  erlang:start_timer(?TICK_RATE, self(), tick).
