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

-export([start_link/1, handle_input/2, get_position/1, get_state/1, set_state/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gproc

name(Id) -> {n, l, {avatar, Id}}.

%% API

start_link(Id) ->
  gen_server:start_link(?MODULE, [Id], []).

handle_input(Id, Point) ->
  case gproc:where(name(Id)) of
    Pid when is_pid(Pid) -> gen_server:cast(Pid, {handle_input, Point});
    _ -> undefined
  end.

get_state(Id) ->
  case gproc:where(name(Id)) of
    Pid when is_pid(Pid) -> gen_server:call(Pid, get_state);
    _ -> undefined
  end.

set_state(Id, State) ->
  case gproc:where(name(Id)) of
    Pid when is_pid(Pid) -> gen_server:cast(Pid, {set_state, State});
    _ -> undefined
  end.

get_position(Id) ->
  case gproc:where(name(Id)) of
    Pid when is_pid(Pid) -> gen_server:call(Pid, get_position);
    _ -> undefined
  end.

%% Callbacks

init([Id]) ->
  lager:info("avatar_server:init(~p)", [Id]),
  gproc:reg(name(Id)),
  Position = pathfinder_server:initial_point(Id),
  lager:info("Position = ~p", [Position]),
  State = #{
    id => Id,
    position => Position,
    movement_speed => 100,
    path => [],
    update_position => true,
    health => 0,
    health_regen => 0,
    mana => 0,
    mana_regen => 0,
    attack_speed => 0,
    attack_range => 0,
    attack_damage => 0,
    state => idle,
    update_state => true,
    xp => 0
  },
  {ok, State, 0}.

handle_call(get_state, _From, State) ->
  {reply, State, State};
handle_call(get_position, _From, #{position := Position} = State) ->
  {reply, Position, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({handle_input, Point}, #{id := Id, position := Position} = State) ->
  lager:info("avatar_server:handle_cast({handle_input, ~p}", Point),
  Blocks = map_tools:blocks(),
  Path = pathfinder_server:path(Id, Position, Point, Blocks),
  lager:info("avatar_server:handle_cast/Path = ~p", [Path]),
  NewState = State#{path => Path},
  {noreply, NewState};
handle_cast({set_state, NewState}, _) ->
  {noreply, NewState};
handle_cast(Request, State) ->
  lager:info("avatar_server:handle_info(~p = Request, State)", [Request]),
  {noreply, State}.

handle_info(timeout, #{id := Id} = State) ->
  lager:info("avatar_server:handle_info(timeout)"),
  map_server:enter(Id),
  {noreply, State};
handle_info(Info, State) ->
  lager:info("avatar_server:handle_info(~p = Info, State)", [Info]),
  {noreply, State}.

terminate(Reason, #{id := Id} = State) ->
  lager:info("avatar_server:terminate(~p = Reason, ~p = State)", [Reason, State]),
  map_server:leave(Id),
  gproc:unreg(name(Id)),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
