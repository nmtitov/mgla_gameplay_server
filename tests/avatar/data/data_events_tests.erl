%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright (C) 2018, N. M. Titov
%%% @doc
%%%
%%% @end
%%% Created : 11. Feb 2018 18:09
%%%-------------------------------------------------------------------
-module(data_events_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").


append_game_event_test_() ->
  D = data_avatar:zero(),
  {Es,_} = data_events:withdraw_processed_events(data_events:process_events(D)),
  E = #{type => autoattack, body => #{from => 0, to => 1, damage => 10}},
  D2 = data_events:add_event(E, D),
  {Es2,D3} = data_events:withdraw_processed_events(data_events:process_events(D2)),
  {Es3,_} = data_events:withdraw_processed_events(data_events:process_events(D3)),
  [
    ?_assertEqual([], Es),
    ?_assertEqual([E], Es2),
    ?_assertEqual([], Es3)
  ].
