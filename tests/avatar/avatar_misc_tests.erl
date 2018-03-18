%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 20:39
%%%-------------------------------------------------------------------
-module(avatar_misc_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

do_is_valid_target_test_() ->
  [
    ?_assertEqual(true, avatar_misc:do_is_valid_target(0, 1)),
    ?_assertEqual(true, avatar_misc:do_is_valid_target(1, 0)),
    ?_assertEqual(false, avatar_misc:do_is_valid_target(0, 0)),
    ?_assertEqual(false, avatar_misc:do_is_valid_target(0, undefined))
  ].

is_valid_target_test_() ->
  D = #{id => 0, target => 1},
  D2 = #{id => 1, target => 0},
  D3 = #{id => 0, target => 0},
  D4 = #{id => 0, target => undefined},
  [
    ?_assertEqual(true, avatar_misc:is_valid_target(D)),
    ?_assertEqual(true, avatar_misc:is_valid_target(D2)),
    ?_assertEqual(false, avatar_misc:is_valid_target(D3)),
    ?_assertEqual(false, avatar_misc:is_valid_target(D4))
  ].