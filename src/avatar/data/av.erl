%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 11:51
%%%-------------------------------------------------------------------
-module(av).
-author("nt").

-export([
  zero/0,
  new/4,

  get_id/1,
  get_name/1,
  get_type/1,

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
  attack_target := av_attack:target(),
  attack_default_cooldown := number(), % second
  attack_cooldown := number(), % seconds
  attack_state := #{
    value := av_attack:state(),
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
    attack_target => undefined,
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

-spec is_dirty(D :: data()) -> boolean().
is_dirty(D) ->
  av_position:get_position_update(D)
  orelse av_position:get_state_update(D)
  orelse av_health:get_health_update(D)
  orelse av_mana:get_mana_update(D)
  orelse av_attack:get_attack_state_update(D).

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
