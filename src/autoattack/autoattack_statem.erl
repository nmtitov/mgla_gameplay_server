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
      case av_misc:is_valid_target(Id, autoattack:get_target(D)) of
        true ->
          D3 = do_attack(D2),
          {keep_state,D3,[{reply,From,ok}]};
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


ready({call,From}, {update,_}, _) ->
  {keep_state_and_data,[{reply,From,ok}]};

ready({call,From}, {set_target,undefined = T} = M, D) ->
  lager:info("~p", [M]),
  D2 = autoattack:set_target(T, D),
  {keep_state,D2,[{reply,From,ok}]};

ready({call,From}, {set_target,T} = M, D) ->
  lager:info("~p", [M]),
  D2 = autoattack:set_target(T, D),
  D3 = do_attack(D2),
  {next_state,cooldown,D3,[{reply,From,ok}]}.


%% Actions

do_attack(D) ->
  TargetId = autoattack:get_target(D),
  lager:info("Attacking #~p", [TargetId]),
  Damage = 10,
  {ok,_} = av_sapi:subtract_health(Damage, TargetId),
  autoattack:trigger_cooldown(D).


-spec set_target(id_server:id_opt(), id_server:id()) -> any().

-spec do_attack(D :: autoattack:data()) -> D2 :: autoattack:data().
