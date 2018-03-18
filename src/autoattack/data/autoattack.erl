%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright (C) 2018, N. M. Titov
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

-type event_type() :: autoattack.

-type game_event() :: #{
  type => event_type(),
  body => map()
}.

-export_type([data/0, game_event/0]).


%% API

-spec new(Id :: id_server:id(), Cooldown :: number()) -> data().
new(Id, Cooldown) -> #{
  id => Id,
  target => undefined,
  cooldown => Cooldown,
  time_left => 0
}.

-spec get_id(D :: data()) -> id_server:id().
get_id(#{id := X}) -> X.

-spec get_target(D :: data()) -> id_server:id_opt().
get_target(#{target := X}) -> X.

-spec set_target(X :: id_server:id_opt(), D :: data()) -> data().
set_target(X, D) ->
  D#{
    target := X
  }.

-spec get_cooldown(D :: data()) -> number().
get_cooldown(#{cooldown := X}) -> X.

-spec get_time_left(D :: data()) -> number().
get_time_left(#{time_left := X}) -> X.

-spec set_time_left(X :: number(), D :: data()) -> data().
set_time_left(X, D) ->
  D#{
    time_left := X
  }.

-spec update(Dt :: number(), D :: data()) -> data().
update(Dt, D) ->
  set_time_left(get_time_left(D) - Dt, D).

-spec is_ready(D :: data()) -> boolean().
is_ready(D) ->
  get_time_left(D) =< 0.

-spec trigger_cooldown(D :: data()) -> data().
trigger_cooldown(D) ->
  case is_ready(D) of false -> error(internal); _ -> ok end,
  Cooldown = get_cooldown(D),
  set_time_left(Cooldown, D).

-spec create_event(D :: data()) -> game_event().
create_event(D) ->
  Id = get_id(D),
  TargetId = get_target(D),
  case TargetId == undefined of true -> error(internal); _ -> ok end,
  #{type => autoattack, body => #{from => Id, to => TargetId, damage => 10}}.
