%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2018 17:58
%%%-------------------------------------------------------------------
-module(av_s).
-author("nt").

-behaviour(gen_server).

-export([name/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gproc

name(Id) -> {n, l, {avatar, Id}}.

%% API

start_link(Type, Id) ->
  gen_server:start_link(av_s, [Type, Id], []).

%% Callbacks

init([Type, Id] = M) ->
  process_flag(trap_exit, true),
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  gproc:reg(name(Id)),
  R = {{0, 0}, {600, 1000}},
  Blocks = map_tools:blocks(),
  Position = pathfinder_server:initial_point(Id, R, Blocks),
  Name = <<"Player">>,
  State = av_d:new(Id, Type, Name, Position),
  {ok, State, 0}.


handle_call(get_data, _From, State) ->
  {reply, State, State};

handle_call(get_position, _From, State) ->
  Position= av_d:get_position_value(State),
  {reply, Position, State};

handle_call(get_state, _From, State) ->
  X = av_d:get_state_value(State),
  {reply, X, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({handle_click, Point} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  Blocks = map_tools:blocks(),
  Id = av_d:get_id(State),
  Position = av_d:get_position_value(State),
  Path = pathfinder_server:path(Id, Position, Point, Blocks),
  NewState = av_d:set_path(Path, State),
  {noreply, NewState};

handle_cast({set_data, NewState}, _) ->
  {noreply, NewState};

handle_cast({set_position, P} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = av_d:set_position_value(P, State),
  {noreply, State2};

handle_cast({add_health, X} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = av_d:update_health_by(X, State),
  {noreply, State2};

handle_cast({subtract_health, X} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = av_d:update_health_by(-X, State),
  {noreply, State2};

handle_cast({add_mana, X} = M, State) ->
  lager:info("avatar_server:handle_cast(~p)", [M]),
  State2 = av_d:update_mana_by(X, State),
  {noreply, State2};

handle_cast({subtract_mana, X} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = av_d:update_mana_by(-X, State),
  {noreply, State2};

handle_cast({set_state, X} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = av_d:set_state_value(X, State),
  {noreply, State2};

handle_cast(Request, State) ->
  lager:info("avatar_server:handle_info(~p = Request, State)", [Request]),
  {noreply, State}.


handle_info(timeout = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  Id = av_d:get_id(State),
  Type = av_d:get_type(State),
  map_server:add_avatar(Type, Id),
  {noreply, State};

handle_info(_Info = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  {noreply, State}.


terminate(_Reason = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  Id = av_d:get_id(State),
  map_server:remove_avatar(Id),
  gproc:unreg(name(Id)),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
