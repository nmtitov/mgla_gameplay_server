%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 11:51
%%%-------------------------------------------------------------------
-module(avatar_data).
-author("nt").

-type data() :: map().
-type state() :: idle | walk.
-type type() :: player | bot.
-export_type([data/0, state/0, type/0]).

%% API
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
  update_health_by/2,
  set_health_max/2,

  get_mana/1,
  set_mana/2,
  get_mana_percent/1,
  update_mana_by/2,
  set_mana_max/2,

  get_state_value/1,
  set_state_value/2,
  get_state_update/1,
  clear_update_flags/1
]).

zero() -> new(0, bot, <<"Zero">>, {0, 0}).

-spec new(Id, Type, Name, Position) -> Data when Id :: non_neg_integer(), Type :: type(), Name :: binary(), Position :: point:point(), Data :: data().
new(Id, Type, Name, Position) ->
  #{
    id => Id,
    type => Type,
    name => Name,
    position => #{
      value => Position,
      update => true
    },
    movement_speed => 100.0,
    path => [],
    health => #{
      value => 100.0,
      update => true
    },
    health_max => #{
      value => 100.0,
      update => true
    },
    health_regen => 0.0,
    mana => #{
      value => 100.0,
      update => true
    },
    mana_max => #{
      value => 100.0,
      update => true
    },
    mana_regen => 0.0,
    attack_speed => 0.0,
    attack_range => 0.0,
    attack_damage => 0.0,
    state => #{
      value => idle,
      update => true
    },
    xp => #{
      value => 0.0,
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
set_health(RawX, #{health := N} = Data) when is_number(RawX) ->
  X = float(RawX),
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
set_health_max(RawXMax, #{health := N, health_max := NMax} = Data) when is_number(RawXMax) ->
  XMax = float(RawXMax),
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

update_health_by(X, Data) -> set_health(get_health(Data) + X, Data).

get_mana(#{mana := #{value := X}}) -> X.
set_mana(RawX, #{mana := N} = Data) when is_number(RawX) ->
  X = float(RawX),
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
set_mana_max(RawXMax, #{mana := N, mana_max := NMax} = Data) when is_number(RawXMax) ->
  XMax = float(RawXMax),
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

update_mana_by(X, Data) -> set_mana(get_mana(Data) + X, Data).

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

-spec get_state_update(Data) -> X when Data :: data(), X :: boolean().
get_state_update(#{state := #{update := X}}) -> X.

-spec clear_update_flags(Data) -> NewData when Data :: data(), NewData :: data().
clear_update_flags(#{position := Position, state := State} = Data) ->
  Data#{
    position := Position#{
      update := false
    },
    state := State#{
      update := false
    }
  }.
