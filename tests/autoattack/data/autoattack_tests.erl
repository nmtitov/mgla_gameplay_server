%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Feb 2018 19:53
%%%-------------------------------------------------------------------
-module(autoattack_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
  D = autoattack:new(0, 5),
  [
    ?_assertEqual(0, autoattack:get_id(D)),
    ?_assertEqual(5, autoattack:get_cooldown(D)),
    ?_assertEqual(0, autoattack:get_time_to_go(D)),
    ?_assertEqual(undefined, autoattack:get_target(D))
  ].

get_id_test_() ->
  D = autoattack:new(0, 5),
  [
    ?_assertEqual(0, autoattack:get_id(D))
  ].

get_cooldown_test_() ->
  D = autoattack:new(0, 5),
  [
    ?_assertEqual(5, autoattack:get_cooldown(D))
  ].

get_time_to_go_test_() ->
  D = autoattack:new(0, 5),
  [
    ?_assertEqual(0, autoattack:get_time_to_go(D))
  ].

get_target_test_() ->
  D = autoattack:new(0, 5),
  [
    ?_assertEqual(undefined, autoattack:get_target(D))
  ].

set_target_test_() ->
  D = autoattack:new(0, 5),
  D2 = autoattack:set_target(0, D),
  D3 = autoattack:set_target(undefined, D),
  [
    ?_assertEqual(undefined, autoattack:get_target(D)),
    ?_assertEqual(0, autoattack:get_target(D2)),
    ?_assertEqual(undefined, autoattack:get_target(D3))
  ].

is_ready_test_() ->
  D = autoattack:new(0, 5),
  D2 = autoattack:trigger_cooldown(D),
  [
    ?_assertEqual(true, autoattack:is_ready(D)),
    ?_assertEqual(false, autoattack:is_ready(D2))
  ].

active_cooldown_test_() ->
  D = autoattack:new(0, 5),
  D2 = autoattack:trigger_cooldown(D),
  [
    ?_assertEqual(0, autoattack:get_time_to_go(D)),
    ?_assertEqual(5, autoattack:get_time_to_go(D2))
  ].