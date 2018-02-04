%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 19:55
%%%-------------------------------------------------------------------
-module(av_d_health).
-author("nt").

%% API
-export([
  get_health/1,
  set_health/2,
  get_health_percent/1,
  get_health_update/1,
  add_health/2,
  subtract_health/2,
  set_health_max/2
]).

-spec get_health(D :: av_d:data()) -> number().
get_health(#{health := #{value := X}}) -> X.

-spec set_health(X :: number(), Data :: av_d:data()) -> av_d:data().
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

-spec get_health_max(D :: av_d:data()) -> number().
get_health_max(#{health_max := #{value := X}}) -> X.

-spec set_health_max(XMax :: number(), D :: av_d:data()) -> av_d:data().
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

-spec get_health_percent(D :: av_d:data()) -> float().
get_health_percent(Data) -> get_health(Data) / get_health_max(Data).

-spec get_health_update(D :: av_d:data()) -> boolean().
get_health_update(#{health := #{update := U}}) -> U.

-spec update_health_by(X :: number(), D :: av_d:data()) -> av_d:data().
update_health_by(X, Data) -> set_health(get_health(Data) + X, Data).

-spec add_health(X :: number(), D :: av_d:data()) -> av_d:data().
add_health(X, Data) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  update_health_by(X, Data).

-spec subtract_health(X :: number(), D :: av_d:data()) -> av_d:data().
subtract_health(X, D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  update_health_by(-X, D).
