%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 11:51
%%%-------------------------------------------------------------------
-module(avatar).
-author("nt").
-include("../../include/avatar_state.hrl").

-type avatar() :: map().

%% API
-export([
  zero/0,
  new/3,
  get_id/1,
  get_name/1,

  get_position_value/1,
  set_position_value/2,
  get_position_update/1,
  get_path/1,
  set_path/2,
  should_move/1,

  get_health_percent/1,
  update_health_by/2,

  get_mana_percent/1,
  update_mana_by/2,

  get_state_value/1,
  set_state_value/2,
  get_state_update/1,
  clear_update_flags/1
]).

zero() -> new(0, <<"Zero">>, {0, 0}).

-spec new(Id, Name, Position) -> Data when Id :: non_neg_integer(), Name :: binary(), Position :: point:point(), Data :: avatar().
new(Id, Name, Position) ->
  #{
    id => Id,
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

-spec get_id(Data) -> X when Data :: avatar(), X :: non_neg_integer().
get_id(#{id := X}) -> X.

-spec get_name(Data) -> X when Data ::avatar(), X :: binary().
get_name(#{name := X}) -> X.

-spec get_position_value(Data) -> X when Data :: avatar(), X :: point:point().
get_position_value(#{position := #{value := X}}) -> X.

-spec set_position_value(X, Data) -> NewData when X :: point:point(), Data :: avatar(), NewData :: avatar().
set_position_value(X, #{position := Nested} = Data) ->
  Data#{
    position := Nested#{
      value := X,
      update := true
    }
  }.

-spec get_position_update(Data) -> X when Data :: avatar(), X :: boolean().
get_position_update(#{position := #{update := X}}) -> X.

-spec get_path(Data) -> X when Data :: avatar(), X :: [point:point()].
get_path(#{path := X}) -> X.

-spec set_path(X, Data) -> NewData when X :: [point:point()], Data :: avatar(), NewData :: avatar().
set_path(X, Data) -> Data#{path := X}.

-spec should_move(Data) -> X when Data :: avatar(), X :: boolean().
should_move(#{path := []}) -> false;
should_move(#{path := _}) -> true.

get_health(#{health := #{value := X}}) -> X.
set_health(X, #{health := N} = Data) ->
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
set_health_max(X, #{health_max := N} = Data) ->
  Data#{
    health_max := N#{
      value := X,
      update := true
    }
  }.

get_health_percent(Data) -> get_health(Data) / get_health_max(Data).

update_health_by(X, Data) -> set_health(get_health(Data) + X, Data).

get_mana(#{mana := #{value := X}}) -> X.
set_mana(X, #{mana := N} = Data) ->
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
set_mana_max(X, #{mana_max := N} = Data) ->
  Data#{
    mana_max := N#{
      value := X,
      update := true
    }
  }.

get_mana_percent(Data) -> get_mana(Data) / get_mana_max(Data).

update_mana_by(X, Data) -> set_mana(get_mana(Data) + X, Data).

-spec get_state_value(Data) -> X when Data :: avatar(), X :: avatar_state().
get_state_value(#{state := #{value := X}}) -> X.

-spec set_state_value(X, Data) -> NewData when X :: avatar_state(), Data :: avatar(), NewData :: avatar().
set_state_value(Value, #{state := Nested} = Data) ->
  Data#{
    state := Nested#{
      value := Value,
      update := true
    }
  }.

-spec get_state_update(Data) -> X when Data :: avatar(), X :: boolean().
get_state_update(#{state := #{update := X}}) -> X.

-spec clear_update_flags(Data) -> NewData when Data :: avatar(), NewData :: avatar().
clear_update_flags(#{position := Position, state := State} = Data) ->
  Data#{
    position := Position#{
      update := false
    },
    state := State#{
      update := false
    }
  }.