-module(gameplay_server_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = {one_for_one, 0, 1},
  ChildSpecs = [#{
    id => id_server,
    start => {id_server, start_link, []}
  }, #{
    id => bot_factory_sup,
    start => {bot_factory_sup, start_link, []},
    type => supervisor
  }, #{
    id => avatar_factory_sup,
    start => {avatar_factory_sup, start_link, []},
    type => supervisor
  }, #{
    id => map_sup,
    start => {map_sup, start_link, []},
    type => supervisor
  }],
  {ok, {SupFlags, ChildSpecs}}.
