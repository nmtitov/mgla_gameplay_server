%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jan 2018 13:26
%%%-------------------------------------------------------------------
-module(pathfinder_server).
-author("nt").

-export([start_link/1, initial_point/1, path/4, next_point/7]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gproc

name(Id) -> {n, l, {pathfinder, Id}}.

%% API

start_link(Id) ->
  gen_server:start_link(?MODULE, [Id], []).

initial_point(Id) ->
  case gproc:where(name(Id)) of
    Pid when is_pid(Pid) -> gen_server:call(Pid, {initial_point});
    _ -> undefined
  end.

path(Id, A, B, Blocks) ->
  case gproc:where(name(Id)) of
    Pid when is_pid(Pid) -> gen_server:call(Pid, {path, A, B, Blocks});
    _ -> undefined
  end.

next_point(Id, A, B, Dt, Speed, MapRect, Blocks) ->
  case gproc:where(name(Id)) of
    Pid when is_pid(Pid) -> gen_server:call(Pid, {next_point, A, B, Dt, Speed, MapRect, Blocks});
    _ -> undefined
  end.

%% Callbacks

init([Id]) ->
  lager:info("pathfinder_server:init(~p)", [Id]),
  gproc:reg(name(Id)),
  {ok, Id}.

handle_call({initial_point}, _From, State) ->
  InitialPoint = path:initial_point(),
  {reply, InitialPoint, State};
handle_call({path, A, B, Blocks}, _From, State) ->
  DestinationPoint = path:path(A, B, Blocks),
  {reply, DestinationPoint, State};
handle_call({next_point, A, B, Dt, Speed, MapRect, Blocks}, _From, State) ->
  NextPoint = path:next_point(A, B, Dt, Speed, MapRect, Blocks),
  {reply, NextPoint, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, Id) ->
  lager:info("pathfinder_server:terminate(~p = Reason, ~p = Id)", [Reason, Id]),
  gproc:unreg(name(Id)),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
