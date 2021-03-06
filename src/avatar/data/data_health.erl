%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright (C) 2018, N. M. Titov
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 19:55
%%%-------------------------------------------------------------------
-module(data_health).
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

-spec get_health(D :: data_avatar:data()) -> number().
get_health(#{health := #{value := X}}) -> X.

-spec set_health(X :: number(), Data :: data_avatar:data()) -> data_avatar:data().
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

-spec get_health_max(D :: data_avatar:data()) -> number().
get_health_max(#{health_max := #{value := X}}) -> X.

-spec set_health_max(XMax :: number(), D :: data_avatar:data()) -> data_avatar:data().
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

-spec get_health_percent(D :: data_avatar:data()) -> float().
get_health_percent(Data) -> get_health(Data) / get_health_max(Data).

-spec get_health_update(D :: data_avatar:data()) -> boolean().
get_health_update(#{health := #{update := U}}) -> U.

-spec add_health(X :: number(), D :: data_avatar:data()) -> data_avatar:data().
add_health(X, D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  set_health(get_health(D) + X, D).

-spec subtract_health(X :: number(), D :: data_avatar:data()) -> data_avatar:data().
subtract_health(X, D) ->
  case X < 0 of true -> error(badarg); _ -> ok end,
  set_health(get_health(D) - X, D).
