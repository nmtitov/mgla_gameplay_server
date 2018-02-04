%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Feb 2018 19:39
%%%-------------------------------------------------------------------
-module(autoattack).
-author("nt").

-export([
  new/2,
  get_id/1,
  get_cooldown/1,
  get_time_to_go/1
]).


-type data() :: #{
  id => id_server:id(),
  cooldown => number(),
  time_to_go => number(),
  target => undefined
}.

-export_type([data/0]).

%% API

new(Id, Cooldown) -> #{
  id => Id,
  cooldown => Cooldown,
  time_to_go => 0,
  target => undefined
}.

get_id(#{id := X}) -> X.
get_cooldown(#{cooldown := X}) -> X.
get_time_to_go(#{time_to_go := X}) -> X.


-spec new(Id :: id_server:id(), Cooldown :: number()) -> data().

-spec get_id(D :: data()) -> id_server:id().
-spec get_cooldown(D :: data()) -> number().
-spec get_time_to_go(D :: data()) -> number().
