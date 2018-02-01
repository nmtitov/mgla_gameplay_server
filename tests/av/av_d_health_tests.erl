%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 18:13
%%%-------------------------------------------------------------------
-module(av_d_health_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

get_health_test_() ->
  Data = av_d:zero(),
  [
    ?_assertEqual(100, av_d_health:get_health(Data))
  ].

get_health_percent_test_() ->
  Data = av_d:zero(),
  [
    ?_assertEqual(1.0, av_d_health:get_health_percent(Data))
  ].

add_health_test_() ->
  D = av_d:zero(),
  D2 = av_d_health:set_health(50, D),
  [
    ?_assertEqual(50, av_d_health:get_health(av_d_health:add_health(0, D2))),
    ?_assertEqual(60, av_d_health:get_health(av_d_health:add_health(10, D2))),
    ?_assertEqual(100, av_d_health:get_health(av_d_health:add_health(50, D2))),
    ?_assertEqual(100, av_d_health:get_health(av_d_health:add_health(100, D2))),
    ?_assertError(badarg, av_d_health:add_health(-50, D2))
  ].

subtract_health_test_() ->
  D = av_d:zero(),
  D2 = av_d_health:set_health(50, D),
  [
    ?_assertEqual(50, av_d_health:get_health(av_d_health:subtract_health(0, D2))),
    ?_assertEqual(40, av_d_health:get_health(av_d_health:subtract_health(10, D2))),
    ?_assertEqual(0, av_d_health:get_health(av_d_health:subtract_health(50, D2))),
    ?_assertEqual(0, av_d_health:get_health(av_d_health:subtract_health(100, D2))),
    ?_assertError(badarg, av_d_health:subtract_health(-50, D2))
  ].

set_health_max_test_() ->
  Data = av_d:zero(),
  Data2 = av_d_health:set_health_max(50, Data),
  Data3 = av_d_health:set_health_max(100, Data2),
  Data4 = av_d_health:set_health(100, Data3),
  [
    ?_assertEqual(50, av_d_health:get_health(Data2)),
    ?_assertEqual(50, av_d_health:get_health(Data3)),
    ?_assertEqual(100, av_d_health:get_health(Data4))
  ].

set_health_test_() ->
  D = av_d:zero(),
  D2 = av_d_health:set_health(50, D),
  D3 = av_d:clear_update_flags(D2),
  D4 = av_d_health:set_health(25, D3),
  [
    ?_assertEqual(50, av_d_health:get_health(D2)),
    ?_assertEqual(true, av_d_health:get_health_update(D2)),
    ?_assertEqual(false, av_d_health:get_health_update(D3)),
    ?_assertEqual(25, av_d_health:get_health(D4)),
    ?_assertEqual(true, av_d_health:get_health_update(D4)),
    ?_assertEqual(true, av_d:is_dirty(D4))
  ].
