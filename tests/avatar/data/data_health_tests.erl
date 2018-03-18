%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright (C) 2018, N. M. Titov
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 18:13
%%%-------------------------------------------------------------------
-module(data_health_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

get_health_test_() ->
  Data = data_avatar:zero(),
  [
    ?_assertEqual(100, data_health:get_health(Data))
  ].

get_health_percent_test_() ->
  Data = data_avatar:zero(),
  [
    ?_assertEqual(1.0, data_health:get_health_percent(Data))
  ].

add_health_test_() ->
  D = data_avatar:zero(),
  D2 = data_health:set_health(50, D),
  [
    ?_assertEqual(50, data_health:get_health(data_health:add_health(0, D2))),
    ?_assertEqual(60, data_health:get_health(data_health:add_health(10, D2))),
    ?_assertEqual(100, data_health:get_health(data_health:add_health(50, D2))),
    ?_assertEqual(100, data_health:get_health(data_health:add_health(100, D2))),
    ?_assertError(badarg, data_health:add_health(-50, D2))
  ].

subtract_health_test_() ->
  D = data_avatar:zero(),
  D2 = data_health:set_health(50, D),
  [
    ?_assertEqual(50, data_health:get_health(data_health:subtract_health(0, D2))),
    ?_assertEqual(40, data_health:get_health(data_health:subtract_health(10, D2))),
    ?_assertEqual(0, data_health:get_health(data_health:subtract_health(50, D2))),
    ?_assertEqual(0, data_health:get_health(data_health:subtract_health(100, D2))),
    ?_assertError(badarg, data_health:subtract_health(-50, D2))
  ].

set_health_max_test_() ->
  Data = data_avatar:zero(),
  Data2 = data_health:set_health_max(50, Data),
  Data3 = data_health:set_health_max(100, Data2),
  Data4 = data_health:set_health(100, Data3),
  [
    ?_assertEqual(50, data_health:get_health(Data2)),
    ?_assertEqual(50, data_health:get_health(Data3)),
    ?_assertEqual(100, data_health:get_health(Data4))
  ].

set_health_test_() ->
  D = data_avatar:zero(),
  D2 = data_health:set_health(50, D),
  D3 = data_avatar:clear_update_flags(D2),
  D4 = data_health:set_health(25, D3),
  [
    ?_assertEqual(50, data_health:get_health(D2)),
    ?_assertEqual(true, data_health:get_health_update(D2)),
    ?_assertEqual(false, data_health:get_health_update(D3)),
    ?_assertEqual(25, data_health:get_health(D4)),
    ?_assertEqual(true, data_health:get_health_update(D4)),
    ?_assertEqual(true, data_avatar:is_dirty(D4))
  ].
