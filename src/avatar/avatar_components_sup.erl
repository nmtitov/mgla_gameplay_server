%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2018 19:10
%%%-------------------------------------------------------------------
-module(avatar_components_sup).
-author("nt").

-behaviour(supervisor).

-export([start_link/1, name/1]).
-export([init/1]).

%% gproc

name(Id) -> {n, l, {?MODULE, Id}}.

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
    shutdown => 5000
  }, #{
    id => autoattack_statem,
    start => {autoattack_statem, start_link, [Id]},
    shutdown => 5000
  }, #{
    id => avatar_server,
    start => {avatar_server, start_link, [player, Id]},
    shutdown => 5000
  }],
  {ok, {RestartStrategy, Children}}.
