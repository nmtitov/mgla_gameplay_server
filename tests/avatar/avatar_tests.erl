%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 13:03
%%%-------------------------------------------------------------------
-module(avatar_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
  Id = 0,
  Position = {0, 0},
  Data = avatar:new(Id, Position),
  NewId = avatar:get_id(Data),
  NewPosition = avatar:get_position_value(Data),
  [
    ?_assert(NewId =:= Id),
    ?_assert(NewPosition =:= Position)
  ].

get_id_test() ->
  Id = 0,
  Position = {0, 0},
  Data = avatar:new(Id, Position),
  NewId = avatar:get_id(Data),
  [
    ?_assert(NewId =:= Id)
  ].

get_position_value_test_() ->
  Id = 0,
  Position = {0, 0},
  Data = avatar:new(Id, Position),
  NewPosition = avatar:get_position_value(Data),
  [
    ?_assert(NewPosition =:= Position)
  ].

set_position_value_test_() ->
  Id = 0,
  Position = {0, 0},
  Data = avatar:new(Id, Position),
  NewPosition = {1, 1},
  NewData = avatar:set_position_value(NewPosition, Data),
  NewPosition2 = avatar:get_position_value(NewData),
  NewPositionUpdate = avatar:get_position_update(NewData),
  [
    ?_assert(NewPosition2 =:= NewPosition),
    ?_assert(NewPositionUpdate =:= true)
  ].

get_path_test_() ->
  Data = avatar:new(0, {0, 0}),
  Path = avatar:get_path(Data),
  [
    ?_assert(Path =:= [])
  ].

set_path_test_() ->
  Data = avatar:new(0, {0, 0}),
  Path = [{0, 0}, {1, 1}, {2, 2}],
  NewData = avatar:set_path(Path, Data),
  NewPath = avatar:get_path(NewData),
  [
    ?_assert(NewPath =:= Path)
  ].

get_state_value_test_() ->
  Id = 0,
  Position = {0, 0},
  Data = avatar:new(Id, Position),
  State = avatar:get_state_value(Data),
  [
    ?_assert(State =:= idle)
  ].

set_state_value_test_() ->
  Data = avatar:new(0, {0, 0}),
  State = walk,
  NewData = avatar:set_state_value(State, Data),
  NewState = avatar:get_state_value(NewData),
  NewStateUpdate = avatar:get_state_update(NewData),
  [
    ?_assert(NewState =:= State),
    ?_assert(NewStateUpdate =:= true)
  ].

clear_update_flags_test() ->
  Data = avatar:new(0, {0, 0}),
  State = walk,
  NewData = avatar:set_state_value(State, Data),
  NewData2 = avatar:clear_update_flags(NewData),
  PositionUpdate = avatar:get_position_update(NewData2),
  StateUpdate = avatar:get_state_update(NewData2),
  [
    ?_assert(PositionUpdate =:= false),
    ?_assert(StateUpdate =:= false)
  ].
