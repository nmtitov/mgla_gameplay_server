%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2018 17:58
%%%-------------------------------------------------------------------
-module(avatar_server).
-author("nt").

-behaviour(gen_server).

-export([name/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gproc

name(Id) -> {n, l, {avatar, Id}}.

%% API

start_link(Type, Id) ->
  gen_server:start_link(avatar_server, [Type, Id], []).

%% Callbacks

init([Type, Id]) ->
  lager:info("avatar_server:init(~p)", [Id]),
  gproc:reg(name(Id)),
  R = {{0, 0}, {600, 1000}},
  Blocks = map_tools:blocks(),
  Position = pathfinder_server:initial_point(Id, R, Blocks),
  Name = <<"Name">>,
  State = avatar_data:new(Id, Type, Name, Position),
  {ok, State, 0}.


handle_call(get_data, _From, State) ->
  {reply, State, State};

handle_call(get_position, _From, State) ->
  Position= avatar_data:get_position_value(State),
  {reply, Position, State};

handle_call(get_state, _From, State) ->
  X = avatar_data:get_state_value(State),
  {reply, X, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({handle_input, Point}, State) ->
  lager:info("avatar_server:handle_cast({handle_input, ~p}", Point),
  Blocks = map_tools:blocks(),
  Id = avatar_data:get_id(State),
  Position = avatar_data:get_position_value(State),
  Path = pathfinder_server:path(Id, Position, Point, Blocks),
  NewState = avatar_data:set_path(Path, State),
  {noreply, NewState};

handle_cast({set_data, NewState}, _) ->
  {noreply, NewState};

handle_cast({set_position, P}, State) ->
  lager:info("avatar_server:handle_cast({set_position, ~p}", P),
  State2 = avatar_data:set_position_value(P, State),
  {noreply, State2};

handle_cast({add_health, X} = M, State) ->
  lager:info("avatar_server:handle_cast(~p)", [M]),
  State2 = avatar_data:update_health_by(X, State),
  {noreply, State2};

handle_cast({subtract_health, X} = M, State) ->
  lager:info("avatar_server:handle_cast(~p)", [M]),
  State2 = avatar_data:update_health_by(-X, State),
  {noreply, State2};

handle_cast({add_mana, X} = M, State) ->
  lager:info("avatar_server:handle_cast(~p)", [M]),
  State2 = avatar_data:update_mana_by(X, State),
  {noreply, State2};

handle_cast({subtract_mana, X} = M, State) ->
  lager:info("avatar_server:handle_cast(~p)", [M]),
  State2 = avatar_data:update_mana_by(-X, State),
  {noreply, State2};

handle_cast({set_state, X} = M, State) ->
  lager:info("avatar_server:handle_cast(~p)", [M]),
  State2 = avatar_data:set_state_value(X, State),
  {noreply, State2};

handle_cast(Request, State) ->
  lager:info("avatar_server:handle_info(~p = Request, State)", [Request]),
  {noreply, State}.


handle_info(timeout, State) ->
  lager:info("avatar_server:handle_info(timeout)"),
  Id = avatar_data:get_id(State),
  Type = avatar_data:get_type(State),
  map_server:add_avatar(Type, Id),
  {noreply, State};

handle_info(Info, State) ->
  lager:info("avatar_server:handle_info(~p = Info, State)", [Info]),
  {noreply, State}.


terminate(Reason, State) ->
  Id = avatar_data:get_id(State),
  lager:info("avatar_server:terminate(~p = Reason, ~p = State)", [Reason, State]),
  map_server:remove_avatar(Id),
  gproc:unreg(name(Id)),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
