-module(ws_handler).
-author("nt").

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
  Id = id_server:id(),
  lager:info("[client_connected] id=~p", [Id]),
  gproc:reg(players_key(Id)),
  gproc:reg(players_broadcast_key()),
  {ok, #state{id = Id}}.

websocket_handle({text, Message}, #state{id = Id} = State) ->
  lager:info("[inc] id=~p: ~p", [Id, Message]),
  Term = jsx:decode(Message),
  lager:info("[dec] id=~p: ~p", [Id, Term]),
  case ws_receive:get_type_and_body(Term) of
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

websocket_info({send, Message}, #state{id = Id} = State) ->
  lager:info("[out] id=~p: ~p", [Id, Message]),
  {reply, {text, Message}, State};
websocket_info(_Info, State) ->
  {ok, State}.

terminate({error, closed}, _Req, #state{id = Id}) ->
  lager:info("[client_disconnected] id=~p", [Id]),
  gproc:unreg(players_key(Id)),
  gproc:unreg(players_broadcast_key()),
  map_server:leave(Id),
  ok;
terminate(Reason, _Req, _State) ->
  io:format("~p~n", [Reason]),
  ok.
