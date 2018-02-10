%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 11:51
%%%-------------------------------------------------------------------
-module(av_position).
-author("nt").

-export([
  get_position_value/1,
  set_position_value/2,
  get_position_update/1,
  get_path/1,
  set_path/2,
  should_move/1,

  get_state_value/1,
  set_state_value/2,
  get_state_update/1
]).


-type state() :: idle | walk.
-export_type([state/0]).


-spec get_position_value(Data) -> X when Data :: av:data(), X :: point:point().
get_position_value(#{position := #{value := X}}) -> X.

-spec set_position_value(X, Data) -> NewData when X :: point:point(), Data :: av:data(), NewData :: av:data().
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

-spec get_position_update(Data) -> X when Data :: av:data(), X :: boolean().
get_position_update(#{position := #{update := X}}) -> X.

-spec get_path(Data) -> X when Data :: av:data(), X :: [point:point()].
get_path(#{path := X}) -> X.

-spec set_path(X, D) -> NewData when X :: [point:point()], D :: av:data(), NewData :: av:data().
set_path(X, D) ->
  D2 = D#{path := X},
  av_attack:clear_target(D2).

-spec should_move(Data) -> X when Data :: av:data(), X :: boolean().
should_move(#{path := []}) -> false;
should_move(#{path := _}) -> true.

-spec get_state_value(Data) -> X when Data :: av:data(), X :: state().
get_state_value(#{state := #{value := X}}) -> X.

-spec set_state_value(X, Data) -> NewData when X :: state(), Data :: av:data(), NewData :: av:data().
set_state_value(Value, #{state := Nested} = Data) ->
  Data#{
    state := Nested#{
      value := Value,
      update := true
    }
  }.

-spec get_state_update(Data :: av:data()) -> boolean().
get_state_update(#{state := #{update := X}}) -> X.