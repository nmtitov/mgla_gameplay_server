%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Feb 2018 18:09
%%%-------------------------------------------------------------------
-module(av_events_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").


append_game_event_test_() ->
  D = av:zero(),
  {Es,_} = av_events:withdraw_processed_events(av_events:process_events(D)),
  E = #{type => autoattack, body => #{from => 0, to => 1, damage => 10}},
  D2 = av_events:add_event(E, D),
  {Es2,D3} = av_events:withdraw_processed_events(av_events:process_events(D2)),
  {Es3,_} = av_events:withdraw_processed_events(av_events:process_events(D3)),
  [
    ?_assertEqual([], Es),
    ?_assertEqual([E], Es2),
    ?_assertEqual([], Es3)
  ].
