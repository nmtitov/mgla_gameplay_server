%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 13:03
%%%-------------------------------------------------------------------
-module(avatar_data_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

zero_test_() ->
  Id = 0,
  Position = {0, 0},
  Data = avatar_data:zero(),
  NewId = avatar_data:get_id(Data),
  Type = avatar_data:get_type(Data),
  NewPosition = avatar_data:get_position_value(Data),
  [
    ?_assertEqual(NewId, Id),
    ?_assertEqual(bot, Type),
    ?_assertEqual(<<"Zero">>, avatar_data:get_name(Data)),
    ?_assertEqual(NewPosition, Position),
    ?_assertEqual(1.0, avatar_data:get_health_percent(Data)),
    ?_assertEqual(1.0, avatar_data:get_mana_percent(Data))
  ].

new_test_() ->
  Id = 0,
  Position = {0, 0},
  Name = <<"New">>,
  Data = avatar_data:new(Id, player, Name, Position),
  NewId = avatar_data:get_id(Data),
  NewPosition = avatar_data:get_position_value(Data),
  NewName = avatar_data:get_name(Data),
  [
    ?_assertEqual(NewId, Id),
    ?_assertEqual(NewPosition, Position),
    ?_assertEqual(Name, NewName)
  ].

get_id_test_() ->
  Id = 0,
  Data = avatar_data:new(Id, player, <<"Test">>, {0, 0}),
  NewId = avatar_data:get_id(Data),
  [
    ?_assertEqual(NewId, Id)
  ].

get_type_test_() ->
  Id = 0,
  Type = player,
  Data = avatar_data:new(Id, Type, <<"Test">>, {0, 0}),
  NewType = avatar_data:get_type(Data),
  [
    ?_assertEqual(NewType, Type)
  ].

get_name_test_() ->
  [
    ?_assertEqual(<<"Test">>, avatar_data:get_name(avatar_data:new(0, player, <<"Test">>, {0, 0})))
  ].


get_position_value_test_() ->
  Position = {0, 0},
  Data = avatar_data:new(0, player, <<"Test">>, Position),
  NewPosition = avatar_data:get_position_value(Data),
  [
    ?_assertEqual(NewPosition, Position)
  ].

set_position_value_test_() ->
  Data = avatar_data:zero(),
  NewPosition = {1, 1},
  NewData = avatar_data:set_position_value(NewPosition, Data),
  NewPosition2 = avatar_data:get_position_value(NewData),
  NewPositionUpdate = avatar_data:get_position_update(NewData),
  [
    ?_assertEqual(NewPosition2, NewPosition),
    ?_assertEqual(NewPositionUpdate, true)
  ].

get_path_test_() ->
  Data = avatar_data:zero(),
  Path = avatar_data:get_path(Data),
  [
    ?_assertEqual(Path, [])
  ].

set_path_test_() ->
  Data = avatar_data:zero(),
  Path = [{0, 0}, {1, 1}, {2, 2}],
  NewData = avatar_data:set_path(Path, Data),
  NewPath = avatar_data:get_path(NewData),
  [
    ?_assertEqual(NewPath, Path)
  ].

should_move_test_() ->
  Data = avatar_data:zero(),
  NewData = avatar_data:set_path([{1, 1}], Data),
  ShouldMoveBefore = avatar_data:should_move(Data),
  ShouldMoveAfter = avatar_data:should_move(NewData),
  [
    ?_assertEqual(ShouldMoveBefore, false),
    ?_assertEqual(ShouldMoveAfter, true)
  ].

get_health_test_() ->
  Data = avatar_data:zero(),
  [
    ?_assertEqual(100.0, avatar_data:get_health(Data))
  ].

get_health_percent_test_() ->
  Data = avatar_data:zero(),
  [
    ?_assertEqual(1.0, avatar_data:get_health_percent(Data))
  ].

update_health_by_test_() ->
  Data = avatar_data:zero(),
  [
    ?_assertEqual(1.0, avatar_data:get_health_percent(avatar_data:update_health_by(50, Data))),
    ?_assertEqual(1.0, avatar_data:get_health_percent(avatar_data:update_health_by(0, Data))),
    ?_assertEqual(0.5, avatar_data:get_health_percent(avatar_data:update_health_by(-50, Data))),
    ?_assertEqual(0.0, avatar_data:get_health_percent(avatar_data:update_health_by(-100, Data))),
    ?_assertEqual(0.0, avatar_data:get_health_percent(avatar_data:update_health_by(-150, Data)))
  ].

set_health_max_test_() ->
  Data = avatar_data:zero(),
  Data2 = avatar_data:set_health_max(50, Data),
  Data3 = avatar_data:set_health_max(100, Data2),
  Data4 = avatar_data:set_health(100, Data3),
  [
    ?_assertEqual(50.0, avatar_data:get_health(Data2)),
    ?_assertEqual(50.0, avatar_data:get_health(Data3)),
    ?_assertEqual(100.0, avatar_data:get_health(Data4))
  ].

get_mana_test_() ->
  Data = avatar_data:zero(),
  [
    ?_assertEqual(100.0, avatar_data:get_mana(Data))
  ].

get_mana_percent_test_() ->
  Data = avatar_data:zero(),
  [
    ?_assertEqual(1.0, avatar_data:get_mana_percent(Data))
  ].

update_mana_by_test_() ->
  Data = avatar_data:zero(),
  [
    ?_assertEqual(1.0, avatar_data:get_mana_percent(avatar_data:update_mana_by(50, Data))),
    ?_assertEqual(1.0, avatar_data:get_mana_percent(avatar_data:update_mana_by(0, Data))),
    ?_assertEqual(0.5, avatar_data:get_mana_percent(avatar_data:update_mana_by(-50, Data))),
    ?_assertEqual(0.0, avatar_data:get_mana_percent(avatar_data:update_mana_by(-100, Data))),
    ?_assertEqual(0.0, avatar_data:get_mana_percent(avatar_data:update_mana_by(-150, Data)))
  ].

set_mana_max_test_() ->
  Data = avatar_data:zero(),
  Data2 = avatar_data:set_mana_max(50, Data),
  Data3 = avatar_data:set_mana_max(100, Data2),
  Data4 = avatar_data:set_mana(100, Data3),
  [
    ?_assertEqual(50.0, avatar_data:get_mana(Data2)),
    ?_assertEqual(50.0, avatar_data:get_mana(Data3)),
    ?_assertEqual(100.0, avatar_data:get_mana(Data4))
  ].

get_state_value_test_() ->
  Data = avatar_data:zero(),
  State = avatar_data:get_state_value(Data),
  [
    ?_assertEqual(State, idle)
  ].

set_state_value_test_() ->
  Data = avatar_data:zero(),
  State = walk,
  NewData = avatar_data:set_state_value(State, Data),
  NewState = avatar_data:get_state_value(NewData),
  NewStateUpdate = avatar_data:get_state_update(NewData),
  [
    ?_assertEqual(NewState, State),
    ?_assertEqual(NewStateUpdate, true)
  ].

clear_update_flags_test() ->
  Data = avatar_data:zero(),
  State = walk,
  NewData = avatar_data:set_state_value(State, Data),
  NewData2 = avatar_data:clear_update_flags(NewData),
  PositionUpdate = avatar_data:get_position_update(NewData2),
  StateUpdate = avatar_data:get_state_update(NewData2),
  [
    ?_assertEqual(PositionUpdate, false),
    ?_assertEqual(StateUpdate, false)
  ].
