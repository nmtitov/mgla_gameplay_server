%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2018 19:15
%%%-------------------------------------------------------------------
-module(avatar_root_sup).
-author("nt").

-behaviour(supervisor).

-export([start_link/0, start_child/1, stop_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% API

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Id) ->
  supervisor:start_child(?SERVER, [Id]).

stop_child(Id) ->
  case gproc:where(avatar_components_sup:name(Id)) of
    Pid when is_pid(Pid) -> {ok, supervisor:terminate_child(?SERVER, Pid)};
    _                    -> {error, undefined}
  end.

%% Callback

init([]) ->
  SupFlags = #{
    strategy => simple_one_for_one,
    intensity => 0,
    period => 1
  },
  ChildSpecs = [#{
    id => avatar_components_sup,
    start => {avatar_components_sup, start_link, []},
    shutdown => brutal_kill
  }],
  {ok, {SupFlags, ChildSpecs}}.