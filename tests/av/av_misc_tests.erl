%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 20:39
%%%-------------------------------------------------------------------
-module(av_misc_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

is_valid_target_test_() ->
  [
    ?_assertEqual(true, av_misc:is_valid_target(0, 1)),
    ?_assertEqual(true, av_misc:is_valid_target(1, 0)),
    ?_assertEqual(false, av_misc:is_valid_target(0, 0)),
    ?_assertEqual(false, av_misc:is_valid_target(0, undefined))
  ].