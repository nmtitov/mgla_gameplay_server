%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 18:13
%%%-------------------------------------------------------------------
-module(av_attack_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

get_speed_test_() ->
  D = av:zero(),
  [
    ?_assertEqual(1.5, av_attack:get_speed(D))
  ].

get_range_test_() ->
  D = av:zero(),
  [
    ?_assertEqual(200, av_attack:get_range(D))
  ].

set_target_test_() ->
  D = av:zero(),
  D2 = av_position:set_path([{1, 1}, {2, 2}], D),
  D3 = av_attack:set_target(0, D2),
  D4 = av_attack:clear_target(D2),
  [
    ?_assertEqual(undefined, av_attack:get_target(D)),
    ?_assertEqual(0, av_attack:get_target(D3)),
    ?_assertEqual([], av_position:get_path(D3)),
    ?_assertEqual(undefined, av_attack:get_target(D4))
  ].