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
  get_time_to_go/1,
  get_target/1,

  set_target/2,

  update/2,
  is_ready/1,
  activate_cooldown/1
]).


-type data() :: #{
  id => id_server:id(),
  cooldown => number(),
  time_to_go => number(),
  target => id_server:id_opt()
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
get_target(#{target := X}) -> X.

set_time_to_go(X, D) ->
  D#{
    time_to_go := X
  }.
set_target(X, D) ->
  D#{
    target := X
  }.


update(Dt, D) ->
  set_time_to_go(get_time_to_go(D) - Dt, D).

is_ready(D) ->
  get_time_to_go(D) =< 0.

activate_cooldown(D) ->
  Cooldown = get_cooldown(D),
  set_time_to_go(Cooldown, D).


-spec new(Id :: id_server:id(), Cooldown :: number()) -> data().

-spec get_id(D :: data()) -> id_server:id().
-spec get_cooldown(D :: data()) -> number().
-spec get_time_to_go(D :: data()) -> number().
-spec get_target(D :: data()) -> id_server:id_opt().

-spec set_time_to_go(X :: number(), D :: data()) -> data().
-spec set_target(X :: id_server:id_opt(), D :: data()) -> data().

-spec update(Dt :: number(), D :: data()) -> data().
-spec is_ready(D :: data()) -> boolean().
-spec activate_cooldown(D :: data()) -> data().
