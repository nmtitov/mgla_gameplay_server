%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright (C) 2018, N. M. Titov
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2018 17:58
%%%-------------------------------------------------------------------
-module(avatar_server).
-author("nt").

-include("../../include/block.hrl").

-behaviour(gen_server).

-export([name/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
  handle_click/3,

  get_data/1,

  get_position/1,

  add_event/2,

  update/4,
  broadcast_update/1,

  clear_update_flags/1
]).


%% gproc

name(Id) -> {n, l, {avatar, Id}}.


%% API

start_link(Type, Id) ->
  gen_server:start_link(avatar_server, [Type, Id], []).

-spec handle_click(Id :: id_server:id(), Point :: point:point(), AvatarId :: id_server:id()) -> ok | gproc_tools:not_found().
handle_click(Id, Point, AvatarId) ->
  gproc_tools:cast(avatar_server:name(Id), {handle_click, Point, AvatarId}).

-spec get_data(Id :: id_server:id()) -> {ok, data_avatar:data()} | gproc_tools:not_found().
get_data(Id) ->
  gproc_tools:call(avatar_server:name(Id), get_data).

-spec get_position(Id :: id_server:id()) -> {ok, point:point()} | gproc_tools:not_found().
get_position(Id) ->
  gproc_tools:call(avatar_server:name(Id), get_position).

-spec add_event(GameEvent :: any(), Id :: id_server:id()) -> {ok, _} | gproc_tools:not_found().
add_event(E, Id) ->
  gproc_tools:call(avatar_server:name(Id), {add_event,E}).

-spec update(Dt :: float(), MapRect :: rect:rect(), Blocks :: [block()], Id :: id_server:id()) -> {ok, _} | gproc_tools:not_found().
update(Dt, MapRect, Blocks, Id) ->
  gproc_tools:call(avatar_server:name(Id), {update, Dt, MapRect, Blocks}).

-spec broadcast_update(Id :: id_server:id()) -> {ok, _} | gproc_tools:not_found().
broadcast_update(Id) ->
  gproc_tools:call(avatar_server:name(Id), broadcast_update).

-spec clear_update_flags(Id :: id_server:id()) -> {ok, data_avatar:data()} | gproc_tools:not_found().
clear_update_flags(Id) ->
  gproc_tools:call(avatar_server:name(Id), clear_update_flags).


%% Callbacks

init([Type, Id] = M) ->
  process_flag(trap_exit, true),
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  gproc:reg(name(Id)),
  R = {{0, 0}, {600, 1000}},
  Blocks = map_tools:blocks(),
  Position = pathfinder_server:initial_point(Id, R, Blocks),
  Name = <<"Player">>,
  State = data_avatar:new(Id, Type, Name, Position),
  {ok, State, 0}.


handle_call(get_data, _From, State) ->
  {reply, State, State};

handle_call(get_position, _From, State) ->
  Position = data_position:get_position_value(State),
  {reply, Position, State};

handle_call(get_state, _From, State) ->
  X = data_position:get_state_value(State),
  {reply, X, State};

handle_call({subtract_health, X}, _From, State) ->
  State2 = data_health:subtract_health(X, State),
  {reply, State2, State2};

handle_call({move, Dt, MapRect, Blocks}, _From, D) ->
  D2 = move(Dt, MapRect, Blocks, D),
  {reply, D2, D2};

handle_call({add_event,E}, _From, D) ->
  D2 = data_events:add_event(E, D),
  {reply, D2, D2};

handle_call(is_dirty, _From, D) ->
  X = data_avatar:is_dirty(D),
  {reply, X, D};

handle_call(clear_update_flags, _From, D) ->
  D2 = data_avatar:clear_update_flags(D),
  {reply, D2, D2};

handle_call({update, Dt, MapRect, Blocks}, _From, D) ->
  Id = data_avatar:get_id(D),
  D2 = move(Dt, MapRect, Blocks, D),
  _AutoattackEvents = autoattack_statem:update(Dt, Id),
  D3 = data_events:process_events(D2),
  {reply, ok, D3};

handle_call(broadcast_update, _From, D) ->
  case data_avatar:is_dirty(D) of
    true -> ws_handler:broadcast(ws_send:update_message(D));
    _ -> ok
  end,
  {reply, ok, D};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({handle_click, Point, AvatarId} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  Blocks = map_tools:blocks(),
  Id = data_avatar:get_id(State),
  Position = data_position:get_position_value(State),
  Range = data_attack:get_range(State),
  State2 = case avatar_misc:do_is_valid_target(Id, AvatarId) andalso avatar_misc:is_in_range(Range, Position, AvatarId) of
    true ->
      autoattack_statem:set_target(AvatarId, Id),
      data_attack:set_target(AvatarId, State);
    _ ->
      autoattack_statem:set_target(undefined, Id),
      Position = data_position:get_position_value(State),
      Path = pathfinder_server:path(Id, Position, Point, Blocks),
      data_position:set_path(Path, State)
  end,
  {noreply, State2};

handle_cast({set_data, NewState}, _) ->
  {noreply, NewState};

handle_cast({set_position, P} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = data_position:set_position_value(P, State),
  {noreply, State2};

handle_cast({add_health, X} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = data_health:add_health(X, State),
  {noreply, State2};

handle_cast({add_mana, X} = M, State) ->
  lager:info("avatar_server:handle_cast(~p)", [M]),
  State2 = data_mana:add_mana(X, State),
  {noreply, State2};

handle_cast({subtract_mana, X} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = data_mana:subtract_mana(X, State),
  {noreply, State2};

handle_cast({set_state, X} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = data_position:set_state_value(X, State),
  {noreply, State2};

handle_cast(Request, State) ->
  lager:info("avatar_server:handle_info(~p = Request, State)", [Request]),
  {noreply, State}.


handle_info(timeout = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  Id = data_avatar:get_id(State),
  Type = data_avatar:get_type(State),
  map_server:add_avatar(Type, Id),
  {noreply, State};

handle_info(_Info = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  {noreply, State}.


terminate(_Reason = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  Id = data_avatar:get_id(State),
  map_server:remove_avatar(Id),
  gproc:unreg(name(Id)),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Private

-spec move(Dt :: float(), MapRect :: rect:rect(), Blocks :: [block()], D :: data_avatar:data()) -> data_avatar:data().
move(_, _, _, #{path := [], state := #{value := State}} = D) ->
  case State of
    walk -> data_position:set_state_value(idle, D);
    _    -> D
  end;
move(Dt, MapRect, Blocks, #{id := Id, position := #{value := A}, path := [B|Rest], movement_speed := S, state := #{value := State}} = Data) ->
  case pathfinder_server:next_point(Id, A, B, S, Dt, MapRect, Blocks) of
    undefined ->
      data_position:set_path(Rest, Data);
    New ->
      case State of
        idle ->
          NewPlayer = data_position:set_position_value(New, Data),
          data_position:set_state_value(walk, NewPlayer);
        _ ->
          data_position:set_position_value(New, Data)
      end
  end.
