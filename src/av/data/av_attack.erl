%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 19:50
%%%-------------------------------------------------------------------
-module(av_attack).
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

-spec get_attack_default_cooldown(D :: av:data()) -> X :: number().
get_attack_default_cooldown(#{attack := #{default_cooldown := X}}) -> X.

-spec set_attack_default_cooldown(X :: number(), D :: av:data()) -> av:data().
set_attack_default_cooldown(X, #{attack := N} = D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  D#{
    attack := N#{
      default_cooldown := X
    }
  }.

-spec get_attack_cooldown(D :: av:data()) -> X :: number().
get_attack_cooldown(#{attack := #{cooldown := X}}) -> X.

-spec set_attack_cooldown(X :: number(), D :: av:data()) -> av:data().
set_attack_cooldown(X, #{attack := #{default_cooldown := DefaultCooldown, cooldown := X} = N} = D) when is_number(X) ->
  D#{
    attack := N#{
      cooldown := if
         X > DefaultCooldown -> DefaultCooldown;
         X < 0 -> 0;
         true -> X
       end
    }
  }.

-spec get_attack_state_value(D) -> X when D :: av:data(), X :: state().
get_attack_state_value(#{attack := #{state := #{value := X}}}) -> X.

-spec set_attack_state_value(X :: state(), D :: av:data()) -> av:data().
set_attack_state_value(X, #{attack := #{state := NN} = N} = D) when is_atom(X) ->
  D#{
    attack := N#{
      state := NN #{
        value := X,
        update := true
      }
    }
  }.

-spec get_attack_state_update(D :: av:data()) -> boolean().
get_attack_state_update(#{attack := #{state := #{update := X}}}) -> X.

-spec get_attack_target(D :: av:data()) -> target().
get_attack_target(#{attack := #{target := X}}) -> X.

-spec set_attack_target(X :: target(), D :: av:data()) -> av:data().
set_attack_target(X, #{attack := N} = D) when is_number(X) ->
  D#{
    attack := N#{
      target := X
    }
  }.

-spec clear_attack_target(D :: av:data()) -> av:data().
clear_attack_target(#{attack := N} = D) ->
  D#{
    attack := N#{
      target := undefined
    }
  }.
