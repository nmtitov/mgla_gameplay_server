%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 20:08
%%%-------------------------------------------------------------------
-module(data_position_tests).
-include_lib("eunit/include/eunit.hrl").
-author("nt").

get_position_value_test_() ->
  Position = {0, 0},
  Data = data_avatar:new(0, player, <<"Test">>, Position),
  NewPosition = data_position:get_position_value(Data),
  [
    ?_assertEqual(NewPosition, Position)
  ].

set_position_value_test_() ->
  Data = data_avatar:zero(),
  NewPosition = {1, 1},
  NewData = data_position:set_position_value(NewPosition, Data),
  NewPosition2 = data_position:get_position_value(NewData),
  NewPositionUpdate = data_position:get_position_update(NewData),
  [
    ?_assertEqual(NewPosition2, NewPosition),
    ?_assertEqual(NewPositionUpdate, true),
    ?_assertEqual(true, data_avatar:is_dirty(NewData))
  ].

get_path_test_() ->
  Data = data_avatar:zero(),
  Path = data_position:get_path(Data),
  [
    ?_assertEqual(Path, [])
  ].

set_path_test_() ->
  D = data_avatar:zero(),
  Path = [{0, 0}, {1, 1}, {2, 2}],
  D2 = data_attack:set_target(0, D),
  D3 = data_position:set_path(Path, D2),
  [
    ?_assertEqual(Path, data_position:get_path(D3)),
    ?_assertEqual(undefined, data_attack:get_target(D3))
  ].

should_move_test_() ->
  Data = data_avatar:zero(),
  NewData = data_position:set_path([{1, 1}], Data),
  ShouldMoveBefore = data_position:should_move(Data),
  ShouldMoveAfter = data_position:should_move(NewData),
  [
    ?_assertEqual(ShouldMoveBefore, false),
    ?_assertEqual(ShouldMoveAfter, true)
  ].

get_state_value_test_() ->
  Data = data_avatar:zero(),
  State = data_position:get_state_value(Data),
  [
    ?_assertEqual(State, idle)
  ].

set_state_value_test_() ->
  Data = data_avatar:zero(),
  State = walk,
  NewData = data_position:set_state_value(State, Data),
  NewState = data_position:get_state_value(NewData),
  NewStateUpdate = data_position:get_state_update(NewData),
  [
    ?_assertEqual(NewState, State),
    ?_assertEqual(NewStateUpdate, true),
    ?_assertEqual(true, data_avatar:is_dirty(NewData))
  ].

