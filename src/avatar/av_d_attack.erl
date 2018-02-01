%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 19:50
%%%-------------------------------------------------------------------
-module(av_d_attack).
-author("nt").

%% API
-export([
  get_attack_default_cooldown/1,
  set_attack_default_cooldown/2,
  get_attack_cooldown/1,
  set_attack_cooldown/2,
  get_attack_state_value/1,
  set_attack_state_value/2,
  get_attack_state_update/1,

  get_attack_target/1,
  set_attack_target/2,
  clear_attack_target/1
]).

-type state() :: idle | attack.
-type target() :: id_server:id() | undefined.
-export_type([state/0, target/0]).

-spec get_attack_default_cooldown(D :: av_d:data()) -> X :: number().
get_attack_default_cooldown(#{attack_default_cooldown := X}) -> X.

-spec set_attack_default_cooldown(X :: number(), D :: av_d:data()) -> av_d:data().
set_attack_default_cooldown(X, D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  D#{
    attack_default_cooldown := X
  }.

-spec get_attack_cooldown(D :: av_d:data()) -> X :: number().
get_attack_cooldown(#{attack_cooldown := X}) -> X.

-spec set_attack_cooldown(X :: number(), D :: av_d:data()) -> av_d:data().
set_attack_cooldown(X, #{attack_default_cooldown := DefaultCooldown, attack_cooldown := X} = D) when is_number(X) ->
  D#{
    attack_cooldown := if
                         X > DefaultCooldown -> DefaultCooldown;
                         X < 0 -> 0;
                         true -> X
                       end
  }.

-spec get_attack_state_value(D) -> X when D :: av_d:data(), X :: state().
get_attack_state_value(#{attack_state := #{value := X}}) -> X.

-spec set_attack_state_value(X :: state(), D :: av_d:data()) -> av_d:data().
set_attack_state_value(X, #{attack_state := N} = D) when is_atom(X) ->
  D#{
    attack_state := N#{
      value := X,
      update := true
    }
  }.

-spec get_attack_state_update(D :: av_d:data()) -> boolean().
get_attack_state_update(#{attack_state := #{update := X}}) -> X.

-spec get_attack_target(D :: av_d:data()) -> target().
get_attack_target(#{attack_target := X}) -> X.

-spec set_attack_target(X :: target(), D :: av_d:data()) -> av_d:data().
set_attack_target(X, D) when is_number(X) ->
  D#{
    attack_target := X
  }.

-spec clear_attack_target(D :: av_d:data()) -> av_d:data().
clear_attack_target(D) ->
  D#{
    attack_target := undefined
  }.
