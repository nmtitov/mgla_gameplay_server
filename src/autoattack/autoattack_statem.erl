%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Feb 2018 12:14
%%%-------------------------------------------------------------------
-module(autoattack_statem).
-author("nt").
-behaviour(gen_statem).

-export([start_link/1,set_target/2, update/2]).
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
-export([cooldown/3,ready/3]).


%% gproc

name(Id) -> {n, l, {autoattack, Id}}.


%% API

start_link(Id) ->
  gen_statem:start_link(autoattack_statem, [Id], []).

set_target(TargetId, Id) ->
  ok = gproc_tools:statem_cast(name(Id), {set_target,TargetId}).

update(Dt, Id) ->
  {ok, GameEvents} = gproc_tools:statem_call(name(Id), {update,Dt}),
  GameEvents.


%% Callback

init([Id]) ->
  process_flag(trap_exit, true),
  gproc:reg(name(Id)),
  D = autoattack:new(Id, 2),
  {ok,ready,D}.

callback_mode() ->
  state_functions.

terminate(_Reason = M, State, D) ->
  Id = autoattack:get_id(D),
  lager:info("~p:~p(~p, ~p, ~p)", [?MODULE, ?FUNCTION_NAME, M, State, D]),
  gproc:unreg(name(Id)),
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.


%%% State

cooldown({call,From}, {update,Dt}, D) ->
  D2 = autoattack:update(Dt, D),
  case autoattack:is_ready(D2) of
    true  ->
      {next_state,ready,D2,[{reply,From,[]}]};
    _ ->
      {keep_state,D2,[{reply,From,[]}]}
  end;
cooldown(EventType, EventContent, D) ->
  handle_event(EventType, EventContent, D).


ready({call,From}, {update,_}, D) ->
  case av_misc:is_valid_target(D) of
    true ->
      GameEvent = do_attack(D),
      D2 = autoattack:trigger_cooldown(D),
      {next_state,cooldown,D2,[{reply,From,[GameEvent]}]};
    _ ->
      {keep_state_and_data,[{reply,From,[]}]}
  end;

ready(EventType, EventContent, D) ->
  handle_event(EventType, EventContent, D).

handle_event(cast, {set_target,T}, D) ->
  D2 = autoattack:set_target(T, D),
  {keep_state,D2,[]}.

%% Actions

do_attack(D) ->
  E = autoattack:create_event(D),
  TargetId = autoattack:get_target(D),
  av_sapi:add_event(E, TargetId),
  E.


-spec set_target(id_server:id_opt(), id_server:id()) -> any().

-spec do_attack(D :: autoattack:data()) -> any().
