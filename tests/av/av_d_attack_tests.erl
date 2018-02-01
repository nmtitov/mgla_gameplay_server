%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 18:13
%%%-------------------------------------------------------------------
-module(av_d_attack_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

set_attack_target_test_() ->
  D = av_d:zero(),
  D2 = av_d_attack:set_attack_target(0, D),
  D3 = av_d_attack:clear_attack_target(D),
  [
    ?_assertEqual(undefined, av_d_attack:get_attack_target(D)),
    ?_assertEqual(0, av_d_attack:get_attack_target(D2)),
    ?_assertEqual(undefined, av_d_attack:get_attack_target(D3))
  ].