%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Feb 2018 18:08
%%%-------------------------------------------------------------------
-module(av_events).
-author("nt").

-export([
  append_game_event/2,
  withdraw_game_events/1,
  handle_game_events/2
]).

-type game_event() :: any().
-type game_events() :: [] | [game_event()].

-export_type([game_event/0, game_events/0]).

%% API

append_game_event(GameEvent, #{game_events := GameEvents} = D) ->
  D#{
    game_events := [GameEvent|GameEvents]
  }.

withdraw_game_events(#{game_events := GameEvents} = D) ->
  D2 = D#{
    game_events := []
  },
  {GameEvents, D2}.


handle_game_events([], D) ->
  D;
handle_game_events([X|Xs], D) ->
  D2 = handle_game_event(X, D),
  handle_game_events(Xs, D2).

handle_game_event(#{type := autoattack, from := _Id, to := _TargetId, damage := Damage}, D) ->
  av_health:subtract_health(Damage, D).


%% Spec

-spec append_game_event(GameEvent :: game_event(), Data :: av:data()) -> av:data().
-spec withdraw_game_events(Data :: av:data()) -> {game_events(), av:data()}.

-spec handle_game_events(GameEvents :: game_events(), D :: av:data()) -> av:data().
-spec handle_game_event(GameEvent :: game_event(), D :: av:data()) -> av:data().
