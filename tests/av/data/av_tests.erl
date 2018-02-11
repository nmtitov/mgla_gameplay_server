%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 13:03
%%%-------------------------------------------------------------------
-module(av_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

zero_test_() ->
  Id = 0,
  Position = {0, 0},
  Data = av:zero(),
  NewId = av:get_id(Data),
  Type = av:get_type(Data),
  NewPosition = av_position:get_position_value(Data),
  [
    ?_assertEqual(NewId, Id),
    ?_assertEqual(bot, Type),
    ?_assertEqual(<<"Zero">>, av:get_name(Data)),
    ?_assertEqual(NewPosition, Position),
    ?_assertEqual(1.0, av_health:get_health_percent(Data)),
    ?_assertEqual(1.0, av_mana:get_mana_percent(Data))
  ].

new_test_() ->
  Id = 0,
  Position = {0, 0},
  Name = <<"New">>,
  Data = av:new(Id, player, Name, Position),
  NewId = av:get_id(Data),
  NewPosition = av_position:get_position_value(Data),
  NewName = av:get_name(Data),
  [
    ?_assertEqual(NewId, Id),
    ?_assertEqual(NewPosition, Position),
    ?_assertEqual(Name, NewName)
  ].

get_id_test_() ->
  Id = 0,
  Data = av:new(Id, player, <<"Test">>, {0, 0}),
  NewId = av:get_id(Data),
  [
    ?_assertEqual(NewId, Id)
  ].

get_type_test_() ->
  Id = 0,
  Type = player,
  Data = av:new(Id, Type, <<"Test">>, {0, 0}),
  NewType = av:get_type(Data),
  [
    ?_assertEqual(NewType, Type)
  ].

get_name_test_() ->
  [
    ?_assertEqual(<<"Test">>, av:get_name(av:new(0, player, <<"Test">>, {0, 0})))
  ].


append_game_event_test_() ->
  D = av:zero(),
  {GE,_} = av:withdraw_game_events(D),
  D2 = av:append_game_event(0, D),
  {GE2,D3} = av:withdraw_game_events(D2),
  {GE3,_} = av:withdraw_game_events(D3),
  [
    ?_assertEqual([], GE),
    ?_assertEqual([0], GE2),
    ?_assertEqual([], GE3)
  ].


clear_update_flags_test() ->
  Data = av:zero(),
  State = walk,
  NewData = av_position:set_state_value(State, Data),
  NewData2 = av:clear_update_flags(NewData),
  PositionUpdate = av_position:get_position_update(NewData2),
  StateUpdate = av_position:get_state_update(NewData2),
  [
    ?_assertEqual(PositionUpdate, false),
    ?_assertEqual(StateUpdate, false),
    ?_assertEqual(false, av:is_dirty(NewData2))
  ].
