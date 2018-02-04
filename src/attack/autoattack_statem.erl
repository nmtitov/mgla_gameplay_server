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
-export([cd/3,ready/3]).

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
  Data = #{
    id => Id,
    init_cd => 5,
    cd => 0,
    target => undefined
  },
  {ok,ready,Data}.

callback_mode() ->
  [state_functions, state_enter].

terminate(_Reason = M, State, #{id := Id} = Data) ->
  lager:info("~p:~p(~p, ~p, ~p)", [?MODULE, ?FUNCTION_NAME, M, State, Data]),
  gproc:unreg(name(Id)),
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.


%%% State

cd(enter, OldState, D) ->
  lager:info("~p:~p(~p, ~p, ~p)", [?MODULE, ?FUNCTION_NAME, enter, OldState, D]),
  {keep_state,D};

cd({call,From}, {update,Dt}, #{cd := Cd} = D) ->
  Cd2 = Cd - Dt,
  D2 = D#{cd := Cd2},
  case Cd2 > 0 of
    true  -> {keep_state,D2,[{reply,From,{ok,D2}}]};
    false -> {next_state,ready,D2,[{reply,From,{ok,D2}}]}
  end;
cd({call,From} = E, {set_target, T} = M, D) ->
  lager:info("~p:~p(~p, ~p)", [?MODULE, ?FUNCTION_NAME, E, M]),
  D2 = D#{target := T},
  {keep_state,D2,[{reply,From,{ok,D2}}]}.


ready(enter, OldState, D) ->
  lager:info("~p:~p(~p, ~p, ~p)", [?MODULE, ?FUNCTION_NAME, enter, OldState, D]),
  {keep_state,D};

ready({call,From} = E, {update,_} = M, #{init_cd := InitCd, target := T} = D) when T =/= undefined ->
  lager:info("~p:~p(~p, ~p)", [?MODULE, ?FUNCTION_NAME, E, M]),
  D2 = D#{cd := InitCd},
  do_attack(T),
  {next_state,cd,D2,[{reply,From,{ok,D2}}]};

ready({call,From}, {update,_}, D) ->
  {keep_state,D,[{reply,From,{ok,D}}]};

ready({call,From}, {set_target,undefined} = M, D) ->
  lager:info("~p", [M]),
  D2 = D#{target := undefined},
  {keep_state,D2,[{reply,From,{ok,D2}}]};

ready({call,From}, {set_target,T} = M, #{init_cd := InitCd} = D) ->
  lager:info("~p", [M]),
  D2 = D#{cd := InitCd, target := T},
  do_attack(T),
  {next_state,cd,D2,[{reply,From,{ok,D2}}]}.


%% Actions

do_attack(TargetId) when TargetId =/= undefined ->
  lager:info("Attacking #~p", [TargetId]),
  Damage = 10,
  {ok,_} = av_sapi:subtract_health(Damage, TargetId).
