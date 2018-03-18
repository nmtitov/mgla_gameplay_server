%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 19:50
%%%-------------------------------------------------------------------
-module(data_attack).
-author("nt").

%% API
-export([
  get_speed/1,
  get_range/1,
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

get_speed(#{attack := #{speed := X}}) -> X.
get_range(#{attack := #{range := X}}) -> X.

-spec get_init_cd(D :: data_avatar:data()) -> X :: number().
get_init_cd(#{attack := #{default_cooldown := X}}) -> X.

-spec set_init_cd(X :: number(), D :: data_avatar:data()) -> data_avatar:data().
set_init_cd(X, #{attack := N} = D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  D#{
    attack := N#{
      default_cooldown := X
    }
  }.

-spec get_cd(D :: data_avatar:data()) -> X :: number().
get_cd(#{attack := #{cooldown := X}}) -> X.

-spec set_cd(X :: number(), D :: data_avatar:data()) -> data_avatar:data().
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

-spec get_state_value(D) -> X when D :: data_avatar:data(), X :: state().
get_state_value(#{attack := #{state := #{value := X}}}) -> X.

-spec set_state_value(X :: state(), D :: data_avatar:data()) -> data_avatar:data().
set_state_value(X, #{attack := #{state := NN} = N} = D) when is_atom(X) ->
  D#{
    attack := N#{
      state := NN #{
        value := X,
        update := true
      }
    }
  }.

-spec get_state_update(D :: data_avatar:data()) -> boolean().
get_state_update(#{attack := #{state := #{update := X}}}) -> X.

-spec get_target(D :: data_avatar:data()) -> target().
get_target(#{attack := #{target := X}}) -> X.

-spec set_target(X :: target(), D :: data_avatar:data()) -> data_avatar:data().
set_target(X, #{attack := N} = D) when is_number(X) ->
  D2 = data_position:set_path([], D),
  D2#{
    attack := N#{
      target := X
    }
  }.

-spec clear_target(D :: data_avatar:data()) -> data_avatar:data().
clear_target(#{attack := N} = D) ->
  D#{
    attack := N#{
      target := undefined
    }
  }.


-spec get_speed(D :: data_avatar:data()) -> X :: number().
-spec get_range(D :: data_avatar:data()) -> X :: number().
