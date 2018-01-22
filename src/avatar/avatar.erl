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
-export([new/2, get_id/1, get_id_position/1, set_path/2, clear_update_flags/1]).

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

get_id(#{id := Id}) -> Id.

get_id_position(#{id := Id, position := #{value := Position}}) -> {Id, Position}.

set_path(Path, State) -> State#{path := Path}.

clear_update_flags(#{position := Position, state := State} = A) ->
  NewPosition = Position#{update := false},
  NewState = State#{update := false},
  A#{
    position := NewPosition,
    state := NewState
  }.