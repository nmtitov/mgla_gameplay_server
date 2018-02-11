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
  withdraw_processed_events/1,
  process_events/1
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

set_processed_events(PrEs, D) ->
  D#{
    processed_events := PrEs
  }.

withdraw_processed_events(#{processed_events := Es} = D) ->
  D2 = D#{
    processed_events := []
  },
  {Es, D2}.


process_events(D) ->
  {Es,D2} = withdraw_events(D),
  {PrEs,D3} = do_process_events(Es, [], D2),
  set_processed_events(PrEs, D3).

do_process_events([], PrEs, D) ->
  {PrEs, D};
do_process_events([E|Es], PrEs, D) ->
  D2 = process_event(E, D),
  do_process_events(Es, [E|PrEs], D2).

process_event(#{type := autoattack, from := _Id, to := _TargetId, damage := Damage}, D) ->
  av_health:subtract_health(Damage, D).


%% Spec

-spec add_event(E :: event(), Data :: av:data()) -> av:data().
-spec withdraw_events(Data :: av:data()) -> {events(), av:data()}.
-spec withdraw_processed_events(Data :: av:data()) -> {events(), av:data()}.

-spec process_events(D :: av:data()) -> av:data().
-spec do_process_events(Es :: events(), PrEs :: events(), D :: av:data()) -> {events(), av:data()}.
-spec process_event(Es :: event(), D :: av:data()) -> av:data().
