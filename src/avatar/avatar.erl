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

%% API
-export([new/2, get_id/1, get_position_value/1, set_position_value/2, get_position_update/1, get_path/1, set_path/2, get_state_value/1, set_state_value/2, get_state_update/1, clear_update_flags/1]).

new(Id, Position) ->
  #{
    id => Id,
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

get_id(#{id := X}) -> X.

get_position_value(#{position := #{value := X}}) -> X.
set_position_value(X, #{position := Nested} = Map) ->
  Map#{
    position := Nested#{
      value := X,
      update := true
    }
  }.

get_position_update(#{position := #{update := X}}) -> X.

get_path(#{path := X}) -> X.
set_path(X, Map) -> Map#{path := X}.

get_state_value(#{state := #{value := X}}) -> X.
set_state_value(Value, #{state := Nested} = Map) ->
  Map#{
    state := Nested#{
      value := Value,
      update := true
    }
  }.

get_state_update(#{state := #{update := X}}) -> X.

clear_update_flags(#{position := Position, state := State} = Map) ->
  Map#{
    position := Position#{
      update := false
    },
    state := State#{
      update := false
    }
  }.