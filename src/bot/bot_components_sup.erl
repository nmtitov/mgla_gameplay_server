%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 22:55
%%%-------------------------------------------------------------------
-module(bot_components_sup).
-author("nt").
-behaviour(supervisor).

-export([start_link/1, name/1]).
-export([init/1]).

%% gproc

name(Id) -> {n, l, {?MODULE, Id}}.

%% API

start_link(Id) ->
  supervisor:start_link(?MODULE, [Id]).

%% Callback

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
    id => bot_server,
    start => {bot_server, start_link, [Id]},
    shutdown => brutal_kill,
    modules => [bot_server]
  }],
  {ok, {RestartStrategy, Children}}.
