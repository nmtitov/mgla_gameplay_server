%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 20:08
%%%-------------------------------------------------------------------
-module(av_d_position_tests).
-include_lib("eunit/include/eunit.hrl").
-author("nt").

get_position_value_test_() ->
  Position = {0, 0},
  Data = av_d:new(0, player, <<"Test">>, Position),
  NewPosition = av_d_position:get_position_value(Data),
  [
    ?_assertEqual(NewPosition, Position)
  ].

set_position_value_test_() ->
  Data = av_d:zero(),
  NewPosition = {1, 1},
  NewData = av_d_position:set_position_value(NewPosition, Data),
  NewPosition2 = av_d_position:get_position_value(NewData),
  NewPositionUpdate = av_d_position:get_position_update(NewData),
  [
    ?_assertEqual(NewPosition2, NewPosition),
    ?_assertEqual(NewPositionUpdate, true),
    ?_assertEqual(true, av_d:is_dirty(NewData))
  ].

get_path_test_() ->
  Data = av_d:zero(),
  Path = av_d_position:get_path(Data),
  [
    ?_assertEqual(Path, [])
  ].

set_path_test_() ->
  Data = av_d:zero(),
  Path = [{0, 0}, {1, 1}, {2, 2}],
  NewData = av_d_position:set_path(Path, Data),
  NewPath = av_d_position:get_path(NewData),
  [
    ?_assertEqual(NewPath, Path)
  ].

should_move_test_() ->
  Data = av_d:zero(),
  NewData = av_d_position:set_path([{1, 1}], Data),
  ShouldMoveBefore = av_d_position:should_move(Data),
  ShouldMoveAfter = av_d_position:should_move(NewData),
  [
    ?_assertEqual(ShouldMoveBefore, false),
    ?_assertEqual(ShouldMoveAfter, true)
  ].

get_state_value_test_() ->
  Data = av_d:zero(),
  State = av_d_position:get_state_value(Data),
  [
    ?_assertEqual(State, idle)
  ].

set_state_value_test_() ->
  Data = av_d:zero(),
  State = walk,
  NewData = av_d_position:set_state_value(State, Data),
  NewState = av_d_position:get_state_value(NewData),
  NewStateUpdate = av_d_position:get_state_update(NewData),
  [
    ?_assertEqual(NewState, State),
    ?_assertEqual(NewStateUpdate, true),
    ?_assertEqual(true, av_d:is_dirty(NewData))
  ].

