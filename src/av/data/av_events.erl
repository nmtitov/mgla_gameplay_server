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
  add_event/2,
  withdraw_events/1,
  process_events/2
]).

-type event() :: any().
-type events() :: [] | [event()].

-export_type([event/0, events/0]).

%% API

add_event(E, #{incoming_events := Es} = D) ->
  D#{
    incoming_events := [E | Es]
  }.

withdraw_events(#{incoming_events := Es} = D) ->
  D2 = D#{
    incoming_events := []
  },
  {Es, D2}.


process_events([], D) ->
  D;
process_events([X|Xs], D) ->
  D2 = process_event(X, D),
  process_events(Xs, D2).

process_event(#{type := autoattack, from := _Id, to := _TargetId, damage := Damage}, D) ->
  av_health:subtract_health(Damage, D).


%% Spec

-spec add_event(E :: event(), Data :: av:data()) -> av:data().
-spec withdraw_events(Data :: av:data()) -> {events(), av:data()}.

-spec process_events(Es :: events(), D :: av:data()) -> av:data().
-spec process_event(Es :: event(), D :: av:data()) -> av:data().
