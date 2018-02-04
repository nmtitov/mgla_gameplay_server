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
  get_init_cd/1,
  set_init_cd/2,
  get_cd/1,
  set_cd/2,
  get_state_value/1,
  set_state_value/2,
  get_state_update/1,

  get_target/1,
  set_target/2,
  clear_target/1
]).

-type state() :: idle | attack.
-type target() :: id_server:id() | undefined.
-export_type([state/0, target/0]).

-spec get_init_cd(D :: av:data()) -> X :: number().
get_init_cd(#{attack := #{default_cooldown := X}}) -> X.

-spec set_init_cd(X :: number(), D :: av:data()) -> av:data().
set_init_cd(X, #{attack := N} = D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  D#{
    attack := N#{
      default_cooldown := X
    }
  }.

-spec get_cd(D :: av:data()) -> X :: number().
get_cd(#{attack := #{cooldown := X}}) -> X.

-spec set_cd(X :: number(), D :: av:data()) -> av:data().
set_cd(X, #{attack := #{default_cooldown := DefaultCooldown, cooldown := X} = N} = D) when is_number(X) ->
  D#{
    attack := N#{
      cooldown := if
         X > DefaultCooldown -> DefaultCooldown;
         X < 0 -> 0;
         true -> X
       end
    }
  }.

-spec get_state_value(D) -> X when D :: av:data(), X :: state().
get_state_value(#{attack := #{state := #{value := X}}}) -> X.

-spec set_state_value(X :: state(), D :: av:data()) -> av:data().
set_state_value(X, #{attack := #{state := NN} = N} = D) when is_atom(X) ->
  D#{
    attack := N#{
      state := NN #{
        value := X,
        update := true
      }
    }
  }.

-spec get_state_update(D :: av:data()) -> boolean().
get_state_update(#{attack := #{state := #{update := X}}}) -> X.

-spec get_target(D :: av:data()) -> target().
get_target(#{attack := #{target := X}}) -> X.

-spec set_target(X :: target(), D :: av:data()) -> av:data().
set_target(X, #{attack := N} = D) when is_number(X) ->
  D#{
    attack := N#{
      target := X
    }
  }.

-spec clear_target(D :: av:data()) -> av:data().
clear_target(#{attack := N} = D) ->
  D#{
    attack := N#{
      target := undefined
    }
  }.
