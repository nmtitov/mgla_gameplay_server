-module(ws_handler).

-export([players_key/1, players_broadcast_key/0]).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(state, {
  id :: id_server:id()
}).

players_key(Id) -> {n, l, {players, Id}}.
players_broadcast_key() -> {p, l, {players, broadcast}}.

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(_) ->
  {ok, Id} = id_server:id(),
  io:format("Client connected: {Id=~p}~n", [Id]),
  gproc:reg(players_key(Id)),
  gproc:reg(players_broadcast_key()),
  {ok, #state{id = Id}}.

websocket_handle({text, M}, #state{id = Id} = State) ->
  Term = jsx:decode(M),
  case ws_receive:get_type_body(Term) of
    {<<"input">>, Body} ->
      P = ws_receive:get_input(Body),
      map_server:input(Id, P);
    {<<"enter">>, _} ->
      map_server:enter(Id);
    {<<"leave">>, _} ->
      map_server:leave(Id)
  end,
  {ok, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({send, Message}, State) ->
  {reply, {text, Message}, State};
websocket_info(_Info, State) ->
  {ok, State}.

terminate({error, closed}, _Req, #state{id = Id}) ->
  io:format("Client disconnected: {Id=~p}~n", [Id]),
  gproc:unreg(players_key(Id)),
  gproc:unreg(players_broadcast_key()),
  map_server:leave(Id),
  ok;
terminate(Reason, _Req, _State) ->
  io:format("~p~n", [Reason]),
  ok.
