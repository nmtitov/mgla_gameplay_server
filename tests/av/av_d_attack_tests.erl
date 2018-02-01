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

smoke_test_() ->
  D = av_d:zero(),
  [
    ?_assertEqual(0, av_d:get_id(D))
  ].