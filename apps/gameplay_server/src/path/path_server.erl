%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jan 2018 13:26
%%%-------------------------------------------------------------------
-module(path_server).
-author("nt").

-export([start_link/0, initial_point/0, next_point/6, path/3]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec initial_point() -> point:point().
initial_point() ->
  gen_server:call(?SERVER, {initial_point}).

-spec path(A, B, Blocks) -> [C] when A :: point:point(), B :: point:point(), Blocks :: [rect:rect()], C :: point:point().
path(A, B, Blocks) ->
  gen_server:call(?SERVER, {path, A, B, Blocks}).

-spec next_point(A, B, Dt, Speed, MapRect, Blocks) -> NextPoint when
  A :: point:point(),
  B :: point:point(),
  Dt :: float(),
  Speed :: float(),
  MapRect :: rect:rect(),
  Blocks :: [rect:rect()],
  NextPoint :: point:point() | undefined.
next_point(A, B, Dt, Speed, MapRect, Blocks) ->
  gen_server:call(?SERVER, {next_point, A, B, Dt, Speed, MapRect, Blocks}).

%% Callbacks

init(_) ->
  {ok, []}.

handle_call({initial_point}, _From, State) ->
  InitialPoint = path:initial_point(),
  {reply, {ok, InitialPoint}, State};
handle_call({path, A, B, Blocks}, _From, State) ->
  DestinationPoint = path:path(A, B, Blocks),
  {reply, {ok, DestinationPoint}, State};
handle_call({next_point, A, B, Dt, Speed, MapRect, Blocks}, _From, State) ->
  NextPoint = path:next_point(A, B, Dt, Speed, MapRect, Blocks),
  {reply, {ok, NextPoint}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
