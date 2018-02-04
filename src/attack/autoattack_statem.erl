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

-export([start/0,update/1,set_target/1,stop/0]).
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
-export([cd/3,ready/3]).

name() -> autoattack_statem. % The registered server name

%% API.  This example uses a registered name name()
%% and does not link to the caller.
start() ->
  gen_statem:start({local,name()}, ?MODULE, [], []).
set_target(TargetId) ->
  gen_statem:call(name(), {set_target,TargetId}).
update(Dt) ->
  gen_statem:call(name(), {update,Dt}).
stop() ->
  gen_statem:stop(name()).

%% Mandatory callback functions
init([]) ->
  Data = #{
    init_cd => 5000,
    cd => 0,
    target => undefined
  },
  {ok,ready,Data}.
callback_mode() ->
  state_functions.
terminate(_Reason, _State, _Data) ->
  void.
code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.

%%% state callback(s)

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

%% actions
do_attack(TargetId) when TargetId =/= undefined ->
  Damage = 1,
  {ok,_} = av_sapi:subtract_health(Damage, TargetId).
