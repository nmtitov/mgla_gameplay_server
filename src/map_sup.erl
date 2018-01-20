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
  RestartStrategy = #{
    strategy => one_for_one,
    intensity => 0,
    period => 1
  },
  Children = [#{
    id => map_server,
    start => {map_server, start_link, []},
    shutdown => brutal_kill,
    modules => [map_server]
  }, #{
    id => path_server,
    start => {path_server, start_link, []},
    shutdown => brutal_kill,
    modules => [path_server]
  }],
  {ok, {RestartStrategy, Children}}.
