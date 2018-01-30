%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 13:03
%%%-------------------------------------------------------------------
-module(av_d_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

zero_test_() ->
  Id = 0,
  Position = {0, 0},
  Data = av_d:zero(),
  NewId = av_d:get_id(Data),
  Type = av_d:get_type(Data),
  NewPosition = av_d:get_position_value(Data),
  [
    ?_assertEqual(NewId, Id),
    ?_assertEqual(bot, Type),
    ?_assertEqual(<<"Zero">>, av_d:get_name(Data)),
    ?_assertEqual(NewPosition, Position),
    ?_assertEqual(1.0, av_d:get_health_percent(Data)),
    ?_assertEqual(1.0, av_d:get_mana_percent(Data))
  ].

new_test_() ->
  Id = 0,
  Position = {0, 0},
  Name = <<"New">>,
  Data = av_d:new(Id, player, Name, Position),
  NewId = av_d:get_id(Data),
  NewPosition = av_d:get_position_value(Data),
  NewName = av_d:get_name(Data),
  [
    ?_assertEqual(NewId, Id),
    ?_assertEqual(NewPosition, Position),
    ?_assertEqual(Name, NewName)
  ].

get_id_test_() ->
  Id = 0,
  Data = av_d:new(Id, player, <<"Test">>, {0, 0}),
  NewId = av_d:get_id(Data),
  [
    ?_assertEqual(NewId, Id)
  ].

get_type_test_() ->
  Id = 0,
  Type = player,
  Data = av_d:new(Id, Type, <<"Test">>, {0, 0}),
  NewType = av_d:get_type(Data),
  [
    ?_assertEqual(NewType, Type)
  ].

get_name_test_() ->
  [
    ?_assertEqual(<<"Test">>, av_d:get_name(av_d:new(0, player, <<"Test">>, {0, 0})))
  ].


get_position_value_test_() ->
  Position = {0, 0},
  Data = av_d:new(0, player, <<"Test">>, Position),
  NewPosition = av_d:get_position_value(Data),
  [
    ?_assertEqual(NewPosition, Position)
  ].

set_position_value_test_() ->
  Data = av_d:zero(),
  NewPosition = {1, 1},
  NewData = av_d:set_position_value(NewPosition, Data),
  NewPosition2 = av_d:get_position_value(NewData),
  NewPositionUpdate = av_d:get_position_update(NewData),
  [
    ?_assertEqual(NewPosition2, NewPosition),
    ?_assertEqual(NewPositionUpdate, true)
  ].

get_path_test_() ->
  Data = av_d:zero(),
  Path = av_d:get_path(Data),
  [
    ?_assertEqual(Path, [])
  ].

set_path_test_() ->
  Data = av_d:zero(),
  Path = [{0, 0}, {1, 1}, {2, 2}],
  NewData = av_d:set_path(Path, Data),
  NewPath = av_d:get_path(NewData),
  [
    ?_assertEqual(NewPath, Path)
  ].

should_move_test_() ->
  Data = av_d:zero(),
  NewData = av_d:set_path([{1, 1}], Data),
  ShouldMoveBefore = av_d:should_move(Data),
  ShouldMoveAfter = av_d:should_move(NewData),
  [
    ?_assertEqual(ShouldMoveBefore, false),
    ?_assertEqual(ShouldMoveAfter, true)
  ].

get_health_test_() ->
  Data = av_d:zero(),
  [
    ?_assertEqual(100.0, av_d:get_health(Data))
  ].

get_health_percent_test_() ->
  Data = av_d:zero(),
  [
    ?_assertEqual(1.0, av_d:get_health_percent(Data))
  ].

update_health_by_test_() ->
  Data = av_d:zero(),
  [
    ?_assertEqual(1.0, av_d:get_health_percent(av_d:update_health_by(50, Data))),
    ?_assertEqual(1.0, av_d:get_health_percent(av_d:update_health_by(0, Data))),
    ?_assertEqual(0.5, av_d:get_health_percent(av_d:update_health_by(-50, Data))),
    ?_assertEqual(0.0, av_d:get_health_percent(av_d:update_health_by(-100, Data))),
    ?_assertEqual(0.0, av_d:get_health_percent(av_d:update_health_by(-150, Data)))
  ].

set_health_max_test_() ->
  Data = av_d:zero(),
  Data2 = av_d:set_health_max(50, Data),
  Data3 = av_d:set_health_max(100, Data2),
  Data4 = av_d:set_health(100, Data3),
  [
    ?_assertEqual(50.0, av_d:get_health(Data2)),
    ?_assertEqual(50.0, av_d:get_health(Data3)),
    ?_assertEqual(100.0, av_d:get_health(Data4))
  ].

set_health_test_() ->
  D = av_d:zero(),
  D2 = av_d:set_health(50.0, D),
  D3 = av_d:clear_update_flags(D2),
  D4 = av_d:set_health(25.0, D3),
  [
    ?_assertEqual(50.0, av_d:get_health(D2)),
    ?_assertEqual(true, av_d:get_health_update(D2)),
    ?_assertEqual(false, av_d:get_health_update(D3)),
    ?_assertEqual(25.0, av_d:get_health(D4)),
    ?_assertEqual(true, av_d:get_health_update(D4))
  ].

get_mana_test_() ->
  Data = av_d:zero(),
  [
    ?_assertEqual(100.0, av_d:get_mana(Data))
  ].

get_mana_percent_test_() ->
  Data = av_d:zero(),
  [
    ?_assertEqual(1.0, av_d:get_mana_percent(Data))
  ].

update_mana_by_test_() ->
  Data = av_d:zero(),
  [
    ?_assertEqual(1.0, av_d:get_mana_percent(av_d:update_mana_by(50, Data))),
    ?_assertEqual(1.0, av_d:get_mana_percent(av_d:update_mana_by(0, Data))),
    ?_assertEqual(0.5, av_d:get_mana_percent(av_d:update_mana_by(-50, Data))),
    ?_assertEqual(0.0, av_d:get_mana_percent(av_d:update_mana_by(-100, Data))),
    ?_assertEqual(0.0, av_d:get_mana_percent(av_d:update_mana_by(-150, Data)))
  ].

set_mana_max_test_() ->
  Data = av_d:zero(),
  Data2 = av_d:set_mana_max(50, Data),
  Data3 = av_d:set_mana_max(100, Data2),
  Data4 = av_d:set_mana(100, Data3),
  [
    ?_assertEqual(50.0, av_d:get_mana(Data2)),
    ?_assertEqual(50.0, av_d:get_mana(Data3)),
    ?_assertEqual(100.0, av_d:get_mana(Data4))
  ].

set_mana_test_() ->
  D = av_d:zero(),
  D2 = av_d:set_mana(50.0, D),
  D3 = av_d:clear_update_flags(D2),
  D4 = av_d:set_mana(25.0, D3),
  [
    ?_assertEqual(50.0, av_d:get_mana(D2)),
    ?_assertEqual(true, av_d:get_mana_update(D2)),
    ?_assertEqual(false, av_d:get_mana_update(D3)),
    ?_assertEqual(25.0, av_d:get_mana(D4)),
    ?_assertEqual(true, av_d:get_mana_update(D4))
  ].

get_state_value_test_() ->
  Data = av_d:zero(),
  State = av_d:get_state_value(Data),
  [
    ?_assertEqual(State, idle)
  ].

set_state_value_test_() ->
  Data = av_d:zero(),
  State = walk,
  NewData = av_d:set_state_value(State, Data),
  NewState = av_d:get_state_value(NewData),
  NewStateUpdate = av_d:get_state_update(NewData),
  [
    ?_assertEqual(NewState, State),
    ?_assertEqual(NewStateUpdate, true)
  ].

clear_update_flags_test() ->
  Data = av_d:zero(),
  State = walk,
  NewData = av_d:set_state_value(State, Data),
  NewData2 = av_d:clear_update_flags(NewData),
  PositionUpdate = av_d:get_position_update(NewData2),
  StateUpdate = av_d:get_state_update(NewData2),
  [
    ?_assertEqual(PositionUpdate, false),
    ?_assertEqual(StateUpdate, false)
  ].
