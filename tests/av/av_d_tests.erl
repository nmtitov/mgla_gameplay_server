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
  NewPosition = av_d_position:get_position_value(Data),
  [
    ?_assertEqual(NewId, Id),
    ?_assertEqual(bot, Type),
    ?_assertEqual(<<"Zero">>, av_d:get_name(Data)),
    ?_assertEqual(NewPosition, Position),
    ?_assertEqual(1.0, av_d_health:get_health_percent(Data)),
    ?_assertEqual(1.0, av_d_mana:get_mana_percent(Data))
  ].

new_test_() ->
  Id = 0,
  Position = {0, 0},
  Name = <<"New">>,
  Data = av_d:new(Id, player, Name, Position),
  NewId = av_d:get_id(Data),
  NewPosition = av_d_position:get_position_value(Data),
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


clear_update_flags_test() ->
  Data = av_d:zero(),
  State = walk,
  NewData = av_d_position:set_state_value(State, Data),
  NewData2 = av_d:clear_update_flags(NewData),
  PositionUpdate = av_d_position:get_position_update(NewData2),
  StateUpdate = av_d_position:get_state_update(NewData2),
  [
    ?_assertEqual(PositionUpdate, false),
    ?_assertEqual(StateUpdate, false),
    ?_assertEqual(false, av_d:is_dirty(NewData2))
  ].
