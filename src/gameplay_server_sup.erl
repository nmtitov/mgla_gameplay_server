-module(gameplay_server_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  IdServer = {id_server, {id_server, start_link, []},
    permanent, 2000, worker, [id_server]},
  MapSup = {map_sup, {map_sup, start_link, []},
    permanent, 2000, worker, [map_sup]},
  PathSup = {path_sup, {path_sup, start_link, []},
    permanent, 2000, worker, [path_sup]},
  Children = [IdServer, MapSup, PathSup],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
