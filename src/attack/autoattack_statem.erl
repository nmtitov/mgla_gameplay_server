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
  {ok, _} = gproc_tools:statem_call(name(Id), {set_target,TargetId}).

update(Dt, Id) ->
  {ok, _} = gproc_tools:statem_call(name(Id), {update,Dt}).


%% Callback

init([Id]) ->
  process_flag(trap_exit, true),
  gproc:reg(name(Id)),
  Data = #{
    id => Id,
    init_cd => 5000,
    cd => 0,
    target => undefined
  },
  {ok,ready,Data}.

callback_mode() ->
  state_functions.

terminate(_Reason = M, State, #{id := Id} = Data) ->
  lager:info("~p:~p(~p, ~p, ~p)", [?MODULE, ?FUNCTION_NAME, M, State, Data]),
  gproc:unreg(name(Id)),
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.


%%% State

cd({call,_}, {update,Dt}, #{cd := Cd} = D) ->
  Cd2 = Cd - Dt,
  D2 = D#{cd := Cd2},
  case Cd2 < 0 of
    true  -> {next_state,ready,D2};
    false -> {keep_state,D2}
  end;
cd({call,_}, {set_target, T}, D) ->
  D2 = D#{target := T},
  {keep_state,D2}.


ready({call,_}, {update,_}, #{init_cd := InitCd, target := T} = D) when T =/= undefined ->
  D2 = D#{cd := InitCd},
  do_attack(T),
  {next_state,cd,D2};

ready(_, {set_target,undefined = T}, D) ->
  D2 = D#{target := T},
  {keep_state,D2};

ready(_, {set_target,T}, #{init_cd := InitCd} = D) ->
  D2 = D#{cd := InitCd, target := T},
  do_attack(T),
  {next_state,cd,D2}.


%% Actions

do_attack(TargetId) when TargetId =/= undefined ->
  lager:info("Attacking #~p", [TargetId]),
  Damage = 1,
  {ok,_} = av_sapi:subtract_health(Damage, TargetId).
