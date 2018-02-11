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

  get_target/1,
  set_target/2,

  get_cooldown/1,
  get_time_left/1,

  update/2,
  is_ready/1,
  trigger_cooldown/1,

  create_event/1
]).


-type data() :: #{
  id => id_server:id(),
  target => id_server:id_opt(),
  cooldown => number(),
  time_left => number()
}.

-type autoattack() :: #{
  id => id_server:id(),
  target => id_server:id_opt()
}.

-type game_event() :: #{
  type => autoattack,
  from => id_server:id(),
  to => id_server:id(),
  damage => number()
}.

-export_type([data/0, autoattack/0, game_event/0]).


%% API

new(Id, Cooldown) -> #{
  id => Id,
  target => undefined,
  cooldown => Cooldown,
  time_left => 0
}.

get_id(#{id := X}) -> X.

get_target(#{target := X}) -> X.
set_target(X, D) ->
  D#{
    target := X
  }.

get_cooldown(#{cooldown := X}) -> X.

get_time_left(#{time_left := X}) -> X.
set_time_left(X, D) ->
  D#{
    time_left := X
  }.

update(Dt, D) ->
  set_time_left(get_time_left(D) - Dt, D).

is_ready(D) ->
  get_time_left(D) =< 0.

trigger_cooldown(D) ->
  case is_ready(D) of false -> error(internal); _ -> ok end,
  Cooldown = get_cooldown(D),
  set_time_left(Cooldown, D).

create_event(D) ->
  Id = get_id(D),
  TargetId = get_target(D),
  case av_misc:is_valid_target(D) of false -> error(internal); _ -> ok end,
  #{type => autoattack, from => Id, to => TargetId, damage => 10}.

-spec new(Id :: id_server:id(), Cooldown :: number()) -> data().

-spec get_id(D :: data()) -> id_server:id().

-spec get_target(D :: data()) -> id_server:id_opt().
-spec set_target(X :: id_server:id_opt(), D :: data()) -> data().

-spec get_cooldown(D :: data()) -> number().

-spec get_time_left(D :: data()) -> number().
-spec set_time_left(X :: number(), D :: data()) -> data().

-spec update(Dt :: number(), D :: data()) -> data().
-spec is_ready(D :: data()) -> boolean().
-spec trigger_cooldown(D :: data()) -> data().

-spec create_event(D :: data()) -> game_event().
