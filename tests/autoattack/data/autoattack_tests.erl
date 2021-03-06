%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright (C) 2018, N. M. Titov
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
    ?_assertEqual(0, autoattack:get_time_left(D)),
    ?_assertEqual(undefined, autoattack:get_target(D))
  ].

get_id_test_() ->
  D = autoattack:new(0, 5),
  [
    ?_assertEqual(0, autoattack:get_id(D))
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

get_cooldown_test_() ->
  D = autoattack:new(0, 5),
  [
    ?_assertEqual(5, autoattack:get_cooldown(D))
  ].

get_time_to_go_test_() ->
  D = autoattack:new(0, 5),
  [
    ?_assertEqual(0, autoattack:get_time_left(D))
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
    ?_assertEqual(0, autoattack:get_time_left(D)),
    ?_assertEqual(5, autoattack:get_time_left(D2))
  ].

create_event_test_() ->
  D = autoattack:new(0, 5),
  D2 = autoattack:set_target(1, D),
  Expected = #{type => autoattack, body => #{from => 0, to => 1, damage => 10}},
  [
    ?_assertError(internal, autoattack:create_event(D)),
    ?_assertEqual(Expected, autoattack:create_event(D2))
  ].
