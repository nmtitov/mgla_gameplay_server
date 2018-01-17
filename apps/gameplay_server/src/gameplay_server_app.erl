-module(gameplay_server_app).
-compile([{parse_transform, lager_transform}]).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  lager:start(),
  Routes = [ {
    '_',
    [
      {"/websocket", ws_handler, []}
    ]
  } ],
  Dispatch = cowboy_router:compile(Routes),

  TransOpts = [{ip, {0,0,0,0}}, {port, 8080}],
  ProtoOpts = #{env => #{dispatch => Dispatch}},

  {ok, _} = cowboy:start_clear(gameplay_server_cowboy_listener, TransOpts, ProtoOpts),
  gameplay_server_sup:start_link().

stop(_State) ->
  ok.
