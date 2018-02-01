%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 11:51
%%%-------------------------------------------------------------------
-module(av_d).
-author("nt").

-export([
  zero/0,
  new/4,

  get_id/1,
  get_name/1,
  get_type/1,

  get_position_value/1,
  set_position_value/2,
  get_position_update/1,
  get_path/1,
  set_path/2,
  should_move/1,

  get_health/1,
  set_health/2,
  get_health_percent/1,
  get_health_update/1,
  add_health/2,
  subtract_health/2,
  set_health_max/2,

  get_mana/1,
  set_mana/2,
  get_mana_percent/1,
  get_mana_update/1,
  add_mana/2,
  subtract_mana/2,
  set_mana_max/2,

  get_attack_default_cooldown/1,
  set_attack_default_cooldown/2,
  get_attack_cooldown/1,
  set_attack_cooldown/2,
  get_attack_state_value/1,
  set_attack_state_value/2,

  get_state_value/1,
  set_state_value/2,
  get_state_update/1,

  is_dirty/1,
  clear_update_flags/1
]).

-type data() :: #{
  id := id_server:id(),
  type := type(),
  name := binary(),
  position := #{
    value := point:point(),
    update := boolean()
  },
  movement_speed := number(),
  path := [point:point()],
  health := #{
    value := number(),
    update := boolean()
  },
  health_max := #{
    value := number(),
    update := boolean()
  },
  health_regen := number(),
  mana := #{
    value := number(),
    update := boolean()
  },
  mana_max := #{
    value := number(),
    update := boolean()
  },
  mana_regen := number(),
  attack_speed := number(),
  attack_range := number(),
  attack_damage := number(),
  attack_default_cooldown := number(), % second
  attack_cooldown := number(), % seconds
  attack_state := #{
    value := attack_state(),
    update := boolean()
  },
  state := #{
    value := state(),
    update := boolean()
  },
  xp := #{
    value := number(),
    update := boolean()
  }
}.
-type state() :: idle | walk.
-type attack_state() :: idle | attack.
-type type() :: player | bot.
-export_type([data/0, state/0, type/0]).

zero() -> new(0, bot, <<"Zero">>, {0, 0}).

-spec new(Id :: non_neg_integer(), Type :: type(), Name :: binary(), Position :: point:point()) -> data().
new(Id, Type, Name, Position) ->
  #{
    id => Id,
    type => Type,
    name => Name,
    position => #{
      value => Position,
      update => true
    },
    movement_speed => 100,
    path => [],
    health => #{
      value => 100,
      update => true
    },
    health_max => #{
      value => 100,
      update => true
    },
    health_regen => 0,
    mana => #{
      value => 100,
      update => true
    },
    mana_max => #{
      value => 100,
      update => true
    },
    mana_regen => 0,
    attack_speed => 0,
    attack_range => 0,
    attack_damage => 0,
    attack_default_cooldown => 5,
    attack_cooldown => 0,
    attack_state => #{
      value => idle,
      update => false
    },
    state => #{
      value => idle,
      update => true
    },
    xp => #{
      value => 0,
      update => true
    }
  }.

-spec get_id(Data) -> X when Data :: data(), X :: non_neg_integer().
get_id(#{id := X}) -> X.

-spec get_type(Data) -> X when Data :: data(), X :: type().
get_type(#{type := X}) -> X.

-spec get_name(Data) -> X when Data :: data(), X :: binary().
get_name(#{name := X}) -> X.

-spec get_position_value(Data) -> X when Data :: data(), X :: point:point().
get_position_value(#{position := #{value := X}}) -> X.

-spec set_position_value(X, Data) -> NewData when X :: point:point(), Data :: data(), NewData :: data().
set_position_value(X, #{position := Nested} = Data) ->
  case point:is_point(X) of
    false  -> error(badarg);
    _      -> ok
  end,
  Data#{
    position := Nested#{
      value := X,
      update := true
    }
  }.

-spec get_position_update(Data) -> X when Data :: data(), X :: boolean().
get_position_update(#{position := #{update := X}}) -> X.

-spec get_path(Data) -> X when Data :: data(), X :: [point:point()].
get_path(#{path := X}) -> X.

-spec set_path(X, Data) -> NewData when X :: [point:point()], Data :: data(), NewData :: data().
set_path(X, Data) -> Data#{path := X}.

-spec should_move(Data) -> X when Data :: data(), X :: boolean().
should_move(#{path := []}) -> false;
should_move(#{path := _}) -> true.

get_health(#{health := #{value := X}}) -> X.

-spec set_health(RawX :: number(), Data :: data()) -> data().
set_health(X, #{health := N} = Data) when is_number(X) ->
  MaxValue = get_health_max(Data),
  Data#{
    health := N#{
      value := if
                 X > MaxValue -> MaxValue;
                 X < 0        -> 0;
                 true         -> X
               end,
      update := true
    }
  }.

get_health_max(#{health_max := #{value := X}}) -> X.
set_health_max(XMax, #{health := N, health_max := NMax} = Data) when is_number(XMax) ->
  X = get_health(Data),
  Data#{
    health := N#{
      value := if
                 X > XMax -> XMax;
                 true     -> X
               end,
      update := true
    },
    health_max := NMax#{
      value := XMax,
      update := true
    }
  }.

get_health_percent(Data) -> get_health(Data) / get_health_max(Data).

-spec get_health_update(D :: data()) -> boolean().
get_health_update(#{health := #{update := U}}) -> U.

update_health_by(X, Data) -> set_health(get_health(Data) + X, Data).

-spec add_health(X :: number(), D :: data()) -> data().
add_health(X, Data) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  update_health_by(X, Data).

-spec subtract_health(X :: number(), D :: data()) -> data().
subtract_health(X, D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  update_health_by(-X, D).

get_mana(#{mana := #{value := X}}) -> X.
set_mana(X, #{mana := N} = Data) when is_number(X) ->
  MaxValue = get_mana_max(Data),
  Data#{
    mana := N#{
      value := if
                 X > MaxValue -> MaxValue;
                 X < 0        -> 0;
                 true         -> X
               end,
      update := true
    }
  }.

get_mana_max(#{mana_max := #{value := X}}) -> X.
set_mana_max(XMax, #{mana := N, mana_max := NMax} = Data) when is_number(XMax) ->
  X = get_mana(Data),
  Data#{
    mana := N#{
      value := if
                 X > XMax -> XMax;
                 true     -> X
               end,
      update := true
    },
    mana_max := NMax#{
      value := XMax,
      update := true
    }
  }.

get_mana_percent(Data) -> get_mana(Data) / get_mana_max(Data).

-spec get_mana_update(D :: data()) -> boolean().
get_mana_update(#{mana := #{update := U}}) -> U.

update_mana_by(X, Data) -> set_mana(get_mana(Data) + X, Data).

-spec add_mana(X :: number(), Data :: data()) -> data().
add_mana(X, D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  update_mana_by(X, D).

-spec subtract_mana(X :: number(), D :: data()) -> data().
subtract_mana(X, D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  update_mana_by(-X, D).


-spec get_attack_default_cooldown(D :: data()) -> X :: number().
get_attack_default_cooldown(#{attack_default_cooldown := X}) -> X.

-spec set_attack_default_cooldown(X :: number(), D :: data()) -> data().
set_attack_default_cooldown(X, D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  D#{
    attack_default_cooldown := X
  }.

-spec get_attack_cooldown(D :: data()) -> X :: number().
get_attack_cooldown(#{attack_cooldown := X}) -> X.

-spec set_attack_cooldown(X :: number(), D :: data()) -> data().
set_attack_cooldown(X, #{attack_default_cooldown := DefaultCooldown, attack_cooldown := X} = D) when is_number(X) ->
  D#{
    attack_cooldown := if
                         X > DefaultCooldown -> DefaultCooldown;
                         X < 0 -> 0;
                         true -> X
                       end
  }.

-spec get_attack_state_value(D) -> X when D :: data(), X :: state().
get_attack_state_value(#{attack_state := #{value := X}}) -> X.

-spec set_attack_state_value(X :: state(), D :: data()) -> data().
set_attack_state_value(X, #{attack_state := N} = D) when is_atom(X) ->
  D#{
    attack_state := N#{
      value := X,
      update := true
    }
  }.

-spec get_attack_state_update(D :: data()) -> boolean().
get_attack_state_update(#{attack_state := #{update := X}}) -> X.

-spec get_state_value(Data) -> X when Data :: data(), X :: state().
get_state_value(#{state := #{value := X}}) -> X.

-spec set_state_value(X, Data) -> NewData when X :: state(), Data :: data(), NewData :: data().
set_state_value(Value, #{state := Nested} = Data) ->
  Data#{
    state := Nested#{
      value := Value,
      update := true
    }
  }.

-spec get_state_update(Data :: data()) -> boolean().
get_state_update(#{state := #{update := X}}) -> X.

-spec is_dirty(D :: data()) -> boolean().
is_dirty(D) ->
  get_position_update(D)
  orelse get_health_update(D)
  orelse get_mana_update(D)
  orelse get_attack_state_update(D)
  orelse get_state_update(D).

-spec clear_update_flags(Data :: data()) -> data().
clear_update_flags(#{position := Position, mana := M, health := H, attack_state := AttackState, state := State} = Data) ->
  Data#{
    position := Position#{
      update := false
    },
    health := H#{
      update := false
    },
    mana := M#{
      update := false
    },
    attack_state := AttackState#{
      update := false
    },
    state := State#{
      update := false
    }
  }.
