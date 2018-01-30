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

-export([start_link/1, initial_point/3, path/4, next_point/7]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gproc

name(Id) -> {n, l, {pathfinder, Id}}.

%% API

start_link(Id) ->
  gen_server:start_link(?MODULE, [Id], []).

initial_point(Id, Rect, Blocks) ->
  {ok, Val} = gproc_tools:call(name(Id), {initial_point, Rect, Blocks}),
  Val.

path(Id, A, B, Blocks) ->
  {ok, Val} = gproc_tools:call(name(Id), {path, A, B, Blocks}),
  Val.

next_point(Id, A, B, Dt, Speed, MapRect, Blocks) ->
  {ok, Val} = gproc_tools:call(name(Id), {next_point, A, B, Dt, Speed, MapRect, Blocks}),
  Val.

%% Callbacks

init([Id]) ->
  process_flag(trap_exit, true),
  lager:info("pathfinder_server:init(~p)", [Id]),
  gproc:reg(name(Id)),
  {ok, Id}.


handle_call({initial_point, Rect, Blocks}, _From, State) ->
  InitialPoint = path:initial_point(Rect, Blocks),
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


terminate(_Reason = M, Id) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  gproc:unreg(name(Id)),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
