-module(survivor_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Routes = [ {
    '_',
    [
      {"/", http_root_handler, []},
      {"/websocket", ws_handler, []}
    ]
  } ],
  Dispatch = cowboy_router:compile(Routes),

  TransOpts = [{ip, {0,0,0,0}}, {port, 8000}],
  ProtoOpts = #{env => #{dispatch => Dispatch}},

  {ok, _} = cowboy:start_clear(survivor, TransOpts, ProtoOpts),
  id_server:start_link(),
  survivor_sup:start_link().

stop(_State) ->
  ok.
