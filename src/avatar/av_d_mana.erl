%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 19:57
%%%-------------------------------------------------------------------
-module(av_d_mana).
-author("nt").

%% API
-export([
  get_mana/1,
  set_mana/2,
  get_mana_percent/1,
  get_mana_update/1,
  add_mana/2,
  subtract_mana/2,
  set_mana_max/2
]).

-spec get_mana(D :: av_d:data()) -> number().
get_mana(#{mana := #{value := X}}) -> X.

-spec set_mana(X :: number(), Data :: av_d:data()) -> av_d:data().
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

-spec get_mana_max(D :: av_d:data()) -> number().
get_mana_max(#{mana_max := #{value := X}}) -> X.

-spec set_mana_max(XMax :: number(), D :: av_d:data()) -> av_d:data().
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

-spec get_mana_percent(D :: av_d:data()) -> float().
get_mana_percent(Data) -> get_mana(Data) / get_mana_max(Data).

-spec get_mana_update(D :: av_d:data()) -> boolean().
get_mana_update(#{mana := #{update := U}}) -> U.

-spec update_mana_by(X :: number(), D :: av_d:data()) -> av_d:data().
update_mana_by(X, Data) -> set_mana(get_mana(Data) + X, Data).

-spec add_mana(X :: number(), Data :: av_d:data()) -> av_d:data().
add_mana(X, D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  update_mana_by(X, D).

-spec subtract_mana(X :: number(), D :: av_d:data()) -> av_d:data().
subtract_mana(X, D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  update_mana_by(-X, D).
