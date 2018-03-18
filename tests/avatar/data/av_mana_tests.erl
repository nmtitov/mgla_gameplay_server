%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 18:13
%%%-------------------------------------------------------------------
-module(av_mana_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

get_mana_test_() ->
  Data = av:zero(),
  [
    ?_assertEqual(100, av_mana:get_mana(Data))
  ].

get_mana_percent_test_() ->
  Data = av:zero(),
  [
    ?_assertEqual(1.0, av_mana:get_mana_percent(Data))
  ].

add_mana_test_() ->
  D = av:zero(),
  D2 = av_mana:set_mana(50, D),
  [
    ?_assertEqual(50, av_mana:get_mana(av_mana:add_mana(0, D2))),
    ?_assertEqual(60, av_mana:get_mana(av_mana:add_mana(10, D2))),
    ?_assertEqual(100, av_mana:get_mana(av_mana:add_mana(50, D2))),
    ?_assertEqual(100, av_mana:get_mana(av_mana:add_mana(100, D2))),
    ?_assertError(badarg, av_mana:add_mana(-50, D2))
  ].

subtract_mana_test_() ->
  D = av:zero(),
  D2 = av_mana:set_mana(50, D),
  [
    ?_assertEqual(50, av_mana:get_mana(av_mana:subtract_mana(0, D2))),
    ?_assertEqual(40, av_mana:get_mana(av_mana:subtract_mana(10, D2))),
    ?_assertEqual(0, av_mana:get_mana(av_mana:subtract_mana(50, D2))),
    ?_assertEqual(0, av_mana:get_mana(av_mana:subtract_mana(100, D2))),
    ?_assertError(badarg, av_mana:subtract_mana(-50, D2))
  ].

set_mana_max_test_() ->
  Data = av:zero(),
  Data2 = av_mana:set_mana_max(50, Data),
  Data3 = av_mana:set_mana_max(100, Data2),
  Data4 = av_mana:set_mana(100, Data3),
  [
    ?_assertEqual(50, av_mana:get_mana(Data2)),
    ?_assertEqual(50, av_mana:get_mana(Data3)),
    ?_assertEqual(100, av_mana:get_mana(Data4))
  ].

set_mana_test_() ->
  D = av:zero(),
  D2 = av_mana:set_mana(50, D),
  D3 = av:clear_update_flags(D2),
  D4 = av_mana:set_mana(25, D3),
  [
    ?_assertEqual(50, av_mana:get_mana(D2)),
    ?_assertEqual(true, av_mana:get_mana_update(D2)),
    ?_assertEqual(false, av_mana:get_mana_update(D3)),
    ?_assertEqual(25, av_mana:get_mana(D4)),
    ?_assertEqual(true, av_mana:get_mana_update(D4)),
    ?_assertEqual(true, av:is_dirty(D4))
  ].
