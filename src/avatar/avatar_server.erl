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
-include("../../include/avatar.hrl").

-behaviour(gen_server).

-export([start_link/2, handle_input/2, get_position/1, set_position/2, dmg_by/2, heal_by/2, get_state/1, set_state/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gproc

name(Id) -> {n, l, {avatar, Id}}.

%% API

start_link(Type, Id) ->
  gen_server:start_link(?MODULE, [Type, Id], []).

-spec handle_input(Id :: id_server:id(), Point :: point:point()) -> ok.
handle_input(Id, Point) ->
  ok = gproc_tools:cast(name(Id), {handle_input, Point}).

-spec get_state(Id :: id_server:id()) -> State :: avatar_data:avatar_data().
get_state(Id) ->
  {ok, State} = gproc_tools:call(name(Id), get_state),
  State.

-spec set_state(Id :: id_server:id(), State :: avatar_data:avatar_data()) -> ok.
set_state(Id, State) ->
  ok = gproc_tools:cast(name(Id), {set_state, State}).

-spec get_position(Id :: id_server:id()) -> point:point().
get_position(Id) ->
  {ok, Value} = gproc_tools:call(name(Id), get_position),
  Value.

-spec set_position(Id :: id_server:id(), P :: point:point()) -> ok.
set_position(Id, P) ->
  ok = gproc_tools:cast(name(Id), {set_position, P}).

-spec dmg_by(Id :: id_server:id(), X :: number()) -> ok.
dmg_by(Id, X) ->
  ok = gproc_tools:cast(name(Id), {dmg_by, X}).

-spec heal_by(Id :: id_server:id(), X :: number()) -> ok.
heal_by(Id, X) ->
  ok = gproc_tools:cast(name(Id), {heal_by, X}).

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


handle_call(get_state, _From, State) ->
  {reply, State, State};

handle_call(get_position, _From, State) ->
  Position= avatar_data:get_position_value(State),
  {reply, Position, State};

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

handle_cast({set_position, P}, State) ->
  lager:info("avatar_server:handle_cast({set_position, ~p}", P),
  State2 = avatar_data:set_position_value(P, State),
  {noreply, State2};

handle_cast({dmg_by, X}, State) ->
  lager:info("avatar_server:handle_cast({dmg_by, ~p}", X),
  State2 = avatar_data:update_health_by(X, State),
  {noreply, State2};

handle_cast({heal_by, X}, State) ->
  lager:info("avatar_server:handle_cast({heal_by, ~p}", X),
  State2 = avatar_data:update_health_by(-X, State),
  {noreply, State2};

handle_cast({set_state, NewState}, _) ->
  {noreply, NewState};

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
