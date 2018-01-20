-module(ws_handler).
-author("nt").

-export([name/1, broadcast_property/0]).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(state, {
  id :: id_server:id()
}).

name(Id) -> {n, l, {ws_handler, Id}}.
broadcast_property() -> {p, l, {ws_handler, broadcast}}.

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(_) ->
  Id = id_server:id(),
  lager:info("[client_connected] id=~p", [Id]),
  gproc:reg(name(Id)),
  gproc:reg(broadcast_property()),
  {ok, #state{id = Id}}.

websocket_handle({text, Message}, #state{id = Id} = State) ->
  lager:info("[received_from_client] id=~p: ~p", [Id, Message]),
  Term = jsx:decode(Message),
  lager:info("[decoded] id=~p: ~p", [Id, Term]),
  case ws_receive:get_type_and_body(Term) of
    {<<"input">>, Body} ->
      Point = ws_receive:get_input(Body),
      avatar_server:handle_input(Id, Point);
    {<<"enter">>, _} ->
      factory_sup:start_child(Id);
    {<<"leave">>, _} ->
      map_server:leave(Id)
  end,
  {ok, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({send, Message}, #state{id = Id} = State) ->
  lager:info("[send_to_client] id=~p: ~p", [Id, Message]),
  {reply, {text, Message}, State};
websocket_info(_Info, State) ->
  {ok, State}.

terminate({error, closed}, _Req, #state{id = Id}) ->
  lager:info("[client_disconnected] id=~p", [Id]),
  factory_sup:stop_child(Id),
  gproc:unreg(name(Id)),
  gproc:unreg(broadcast_property()),
  ok;
terminate(Reason, _Req, _State) ->
  io:format("~p~n", [Reason]),
  ok.
