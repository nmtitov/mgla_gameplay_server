%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright © 2018 N. M. Titov. All rights reserved.
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2018 22:39
%%%-------------------------------------------------------------------
-module(map_sup).
-author("nt").
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Server = {map_server, {map_server, start_link, []},
            permanent, 2000, worker, [map_server]},
  Children = [Server],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
