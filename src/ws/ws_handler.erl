-module(ws_handler).
-author("nt").

-export([send/2, broadcast/1]).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

name(Id) -> {n, l, {ws_handler, Id}}.
broadcast_property() -> {p, l, {ws_handler, broadcast}}.


%% API

send(Id, Message) ->
  gproc:send(name(Id), {send, Message}).

broadcast(Message) ->
  gproc:send(broadcast_property(), {send, Message}).


%% Callbacks

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.


websocket_init(Params) ->
  Id = id_server:get_id(),
  lager:info("~p:~p/~p(~p)", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Params]),
  lager:info("connect id=~p", [Id]),
  gproc:reg(name(Id)),
  gproc:reg(broadcast_property()),
  {ok, #{id => Id}}.


websocket_handle({text, Message} = Info, #{id := Id} = State) ->
  lager:info("~p:~p/~p(~p, ~p)", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Info, State]),
  lager:info("<<id=~p", [Id]),
  Term = jsx:decode(Message),
  lager:info("[decoded] id=~p: ~p", [Id, Term]),
  case ws_receive:get_type_and_body(Term) of
    {<<"input">>, Body} ->
      Point = ws_receive:get_input(Body),
      avatar_sapi:handle_input(Id, Point);
    {<<"enter">>, _} ->
      avatar_factory_sup:start_child(Id);
    {<<"leave">>, _} ->
      map_server:remove_avatar(Id)
  end,
  {ok, State};

websocket_handle(_Data, State) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  {ok, State}.


websocket_info({send, Message} = _Info, #{id := _Id} = State) ->
%%  lager:info("~p:~p/~p(~p, ~p)", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Info, State]),
%%  lager:info(">>id=~p", [Id]),
  {reply, {text, Message}, State};

websocket_info(_Info, State) ->
%%  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  {ok, State}.


terminate({error, closed} = Info, _Req, #{id := Id} = State) ->
  lager:info("~p:~p ~p:~p/~p(~p, ~p)", [?FILE, ?LINE, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Info, State]),
  lager:info("exit id=~p", [Id]),
  avatar_factory_sup:stop_child(Id),
  gproc:unreg(name(Id)),
  gproc:unreg(broadcast_property()),
  ok;

terminate(Reason, _Req, State) ->
  lager:info("~p:~p ~p:~p/~p(~p, ~p)", [?FILE, ?LINE, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Reason, State]),
  ok.
