%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2018 19:10
%%%-------------------------------------------------------------------
-module(components_sup).
-author("nt").

-behaviour(supervisor).

-export([start_link/1, name/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% gproc

name(Id) -> {n, l, {components_sup, Id}}.

start_link(Id) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Id]).

init([Id]) ->
  gproc:reg(name(Id)),
  RestartStrategy = #{
    strategy => one_for_one,
    intensity => 0,
    period => 1
  },
  Children = [#{
    id => pathfinder_server,
    start => {pathfinder_server, start_link, [Id]},
    shutdown => brutal_kill,
    modules => [pathfinder_server]
  }, #{
    id => avatar_server,
    start => {avatar_server, start_link, [Id]},
    shutdown => brutal_kill,
    modules => [avatar_server]
  }],
  {ok, {RestartStrategy, Children}}.
