%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 13:03
%%%-------------------------------------------------------------------
-module(data_avatar_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").


zero_test_() ->
  Id = 0,
  Position = {0, 0},
  Data = data_avatar:zero(),
  NewId = data_avatar:get_id(Data),
  Type = data_avatar:get_type(Data),
  NewPosition = data_position:get_position_value(Data),
  [
    ?_assertEqual(NewId, Id),
    ?_assertEqual(bot, Type),
    ?_assertEqual(<<"Zero">>, data_avatar:get_name(Data)),
    ?_assertEqual(NewPosition, Position),
    ?_assertEqual(1.0, data_health:get_health_percent(Data)),
    ?_assertEqual(1.0, data_mana:get_mana_percent(Data))
  ].

new_test_() ->
  Id = 0,
  Position = {0, 0},
  Name = <<"New">>,
  Data = data_avatar:new(Id, player, Name, Position),
  NewId = data_avatar:get_id(Data),
  NewPosition = data_position:get_position_value(Data),
  NewName = data_avatar:get_name(Data),
  [
    ?_assertEqual(NewId, Id),
    ?_assertEqual(NewPosition, Position),
    ?_assertEqual(Name, NewName)
  ].

get_id_test_() ->
  Id = 0,
  Data = data_avatar:new(Id, player, <<"Test">>, {0, 0}),
  NewId = data_avatar:get_id(Data),
  [
    ?_assertEqual(NewId, Id)
  ].

get_type_test_() ->
  Id = 0,
  Type = player,
  Data = data_avatar:new(Id, Type, <<"Test">>, {0, 0}),
  NewType = data_avatar:get_type(Data),
  [
    ?_assertEqual(NewType, Type)
  ].

get_name_test_() ->
  [
    ?_assertEqual(<<"Test">>, data_avatar:get_name(data_avatar:new(0, player, <<"Test">>, {0, 0})))
  ].


clear_update_flags_test() ->
  Data = data_avatar:zero(),
  State = walk,
  NewData = data_position:set_state_value(State, Data),
  NewData2 = data_avatar:clear_update_flags(NewData),
  PositionUpdate = data_position:get_position_update(NewData2),
  StateUpdate = data_position:get_state_update(NewData2),
  [
    ?_assertEqual(PositionUpdate, false),
    ?_assertEqual(StateUpdate, false),
    ?_assertEqual(false, data_avatar:is_dirty(NewData2))
  ].
