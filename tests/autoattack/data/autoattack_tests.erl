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