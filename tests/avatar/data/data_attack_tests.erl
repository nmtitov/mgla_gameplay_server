%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright (C) 2018, N. M. Titov
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 18:13
%%%-------------------------------------------------------------------
-module(data_attack_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

get_speed_test_() ->
  D = data_avatar:zero(),
  [
    ?_assertEqual(1.5, data_attack:get_speed(D))
  ].

get_range_test_() ->
  D = data_avatar:zero(),
  [
    ?_assertEqual(200, data_attack:get_range(D))
  ].

set_target_test_() ->
  D = data_avatar:zero(),
  D2 = data_position:set_path([{1, 1}, {2, 2}], D),
  D3 = data_attack:set_target(0, D2),
  D4 = data_attack:clear_target(D2),
  [
    ?_assertEqual(undefined, data_attack:get_target(D)),
    ?_assertEqual(0, data_attack:get_target(D3)),
    ?_assertEqual([], data_position:get_path(D3)),
    ?_assertEqual(undefined, data_attack:get_target(D4))
  ].