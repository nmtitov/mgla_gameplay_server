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
  lager:info("~p:~p(~p, ~p)", [?MODULE, ?FUNCTION_NAME, TargetId, Id]),
  {ok, _} = gproc_tools:statem_call(name(Id), {set_target,TargetId}).

update(Dt, Id) ->
  {ok, _} = gproc_tools:statem_call(name(Id), {update,Dt}).


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
  lager:info("~p:~p(~p, ~p, ~p)", [?MODULE, ?FUNCTION_NAME, M, State, Id]),
  gproc:unreg(name(Id)),
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.


%%% State

cooldown({call,From}, {update,Dt}, D) ->
  Id = autoattack:get_id(D),
  D2 = autoattack:update(Dt, D),
  case autoattack:is_ready(D2) of
    true  ->
      Target = autoattack:get_target(D),
      case av_misc:is_valid_target(Id, Target) of
        true ->
          D3 = autoattack:activate_cooldown(D2),
          do_attack(Target),
          {keep_state,D3,[{reply,From,{ok,D3}}]};
        false ->
          {next_state,ready,D2,[{reply,From,{ok,D2}}]}
      end;
    false ->
      {keep_state,D2,[{reply,From,{ok,D2}}]}
  end;
cooldown({call,From} = E, {set_target,T} = M, D) ->
  lager:info("~p:~p(~p, ~p)", [?MODULE, ?FUNCTION_NAME, E, M]),
  D2 = autoattack:set_target(T, D),
  {keep_state,D2,[{reply,From,{ok,D2}}]}.


ready({call,From}, {update,_}, D) ->
  {keep_state,D,[{reply,From,{ok,D}}]};

ready({call,From}, {set_target,undefined = T} = M, D) ->
  lager:info("~p", [M]),
  D2 = autoattack:set_target(T, D),
  {keep_state,D2,[{reply,From,{ok,D2}}]};

ready({call,From}, {set_target,T} = M, D) ->
  lager:info("~p", [M]),
  D2 = autoattack:set_target(T, D),
  D3 = autoattack:activate_cooldown(D2),
  do_attack(T),
  {next_state, cooldown,D3,[{reply,From,{ok,D3}}]}.


%% Actions

do_attack(TargetId) ->
  lager:info("Attacking #~p", [TargetId]),
  Damage = 10,
  {ok,_} = av_sapi:subtract_health(Damage, TargetId).
