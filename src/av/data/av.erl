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
  attack := #{
    speed := number(),
    range := number(),
    damage := number(),
    target := av_attack:target(),
    default_cooldown := number(), % second
    cooldown := number(), % seconds
    state := #{
      value := av_attack:state(),
      update := boolean()
    }
  },
  state := #{
    value := state(),
    update := boolean()
  },
  xp := #{
    value := number(),
    update := boolean()
  },
  incoming_events := av_events:events(),
  processed_events := av_events:events()
}.
-type state() :: idle | walk.
-type type() :: player | bot.
-export_type([data/0, state/0, type/0]).

zero() -> new(0, bot, <<"Zero">>, {0, 0}).

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
    attack => #{
      speed => 1.5,
      range => 200,
      damage => 0,
      target => undefined,
      default_cooldown => 5,
      cooldown => 0,
      state => #{
        value => idle,
        update => false
      }
    },
    state => #{
      value => idle,
      update => true
    },
    xp => #{
      value => 0,
      update => true
    },
    incoming_events => [],
    processed_events => []
  }.

get_id(#{id := X}) -> X.
get_type(#{type := X}) -> X.
get_name(#{name := X}) -> X.


is_dirty(D) ->
  av_position:get_position_update(D)
  orelse av_position:get_state_update(D)
  orelse av_health:get_health_update(D)
  orelse av_mana:get_mana_update(D)
  orelse av_attack:get_state_update(D).

clear_update_flags(#{position := Position, mana := M, health := H, attack := #{state := AS} = A, state := State} = Data) ->
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
    attack := A#{
      state := AS#{
        update := false
      }
    },
    state := State#{
      update := false
    }
  }.


%% Spec

-spec new(Id :: non_neg_integer(), Type :: type(), Name :: binary(), Position :: point:point()) -> data().

-spec get_id(Data) -> X when Data :: data(), X :: non_neg_integer().
-spec get_type(Data) -> X when Data :: data(), X :: type().
-spec get_name(Data) -> X when Data :: data(), X :: binary().

-spec is_dirty(D :: data()) -> boolean().
-spec clear_update_flags(Data :: data()) -> data().
