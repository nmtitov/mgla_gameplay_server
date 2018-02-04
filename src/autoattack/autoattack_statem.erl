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
-export([cooldown/3,ready/3,exec/3]).

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
      case av_misc:is_valid_target(Id, autoattack:get_target(D)) of
        true ->
          {next_state,exec,D2,[{reply,From,ok},{state_timeout,0,cooldown}]};
        _ ->
          {next_state,ready,D2,[{reply,From,ok}]}
      end;
    _ ->
      {keep_state,D2,[{reply,From,ok}]}
  end;
cooldown({call,From} = E, {set_target,T} = M, D) ->
  lager:info("~p:~p(~p, ~p)", [?MODULE, ?FUNCTION_NAME, E, M]),
  D2 = autoattack:set_target(T, D),
  {keep_state,D2,[{reply,From,ok}]}.


ready({call,From}, {update,_}, D) ->
  {keep_state_and_data,[{reply,From,ok}]};

ready({call,From}, {set_target,undefined = T} = M, D) ->
  lager:info("~p", [M]),
  D2 = autoattack:set_target(T, D),
  {keep_state,D2,[{reply,From,ok}]};

ready({call,From}, {set_target,T} = M, D) ->
  lager:info("~p", [M]),
  D2 = autoattack:set_target(T, D),
  {next_state,exec,D2,[{reply,From,ok},{state_timeout,0,cooldown}]}.


exec(state_timeout, cooldown, D) ->
  T = autoattack:get_target(D),
  do_attack(T),
  D2 = autoattack:activate_cooldown(D),
  {next_state,cooldown,D2,[]}.


%% Actions

do_attack(TargetId) ->
  lager:info("Attacking #~p", [TargetId]),
  Damage = 10,
  {ok,_} = av_sapi:subtract_health(Damage, TargetId).
