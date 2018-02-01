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

-include("../../include/block.hrl").

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
  Position = av_d_position:get_position_value(State),
  {reply, Position, State};

handle_call(get_state, _From, State) ->
  X = av_d_position:get_state_value(State),
  {reply, X, State};

handle_call({subtract_health, X} = M, _From, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = av_d_health:subtract_health(X, State),
  lager:info("New health ~p", [av_d_health:get_health(State2)]),
  lager:info("New health% ~p", [av_d_health:get_health_percent(State2)]),
  {reply, State2, State2};

handle_call({move, Dt, MapRect, Blocks}, _From, D) ->
  D2 = move(Dt, MapRect, Blocks, D),
  {reply, D2, D2};

handle_call(is_dirty, _From, D) ->
  X = av_d:is_dirty(D),
  {reply, X, D};

handle_call(clear_update_flags, _From, D) ->
  D2 = av_d:clear_update_flags(D),
  {reply, D2, D2};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({handle_click, Point, AvatarId} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  Blocks = map_tools:blocks(),
  Id = av_d:get_id(State),
  NewState = case av_misc:is_valid_target(Id, AvatarId) of
    true ->
      map_server:attack(Id, AvatarId),
      State2 = av_d_attack:set_attack_target(AvatarId, State),
      TargetId = av_d_attack:get_attack_target(State2),
      lager:info("New target: ~p", [TargetId]),
      State2;
    _ ->
      Position = av_d_position:get_position_value(State),
      Path = pathfinder_server:path(Id, Position, Point, Blocks),
      State2 = av_d_position:set_path(Path, State),
      State3 = av_d_attack:clear_attack_target(State2),
      TargetId = av_d_attack:get_attack_target(State2),
      lager:info("New target: ~p", [TargetId]),
      State3
  end,
  {noreply, NewState};

handle_cast({set_data, NewState}, _) ->
  {noreply, NewState};

handle_cast({set_position, P} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = av_d_position:set_position_value(P, State),
  {noreply, State2};

handle_cast({add_health, X} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = av_d_health:add_health(X, State),
  {noreply, State2};

handle_cast({add_mana, X} = M, State) ->
  lager:info("avatar_server:handle_cast(~p)", [M]),
  State2 = av_d_mana:add_mana(X, State),
  {noreply, State2};

handle_cast({subtract_mana, X} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = av_d_mana:subtract_mana(X, State),
  {noreply, State2};

handle_cast({set_state, X} = M, State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  State2 = av_d_position:set_state_value(X, State),
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


%% Private

-spec move(Dt :: float(), MapRect :: rect:rect(), Blocks :: [block()], D :: av_d:data()) -> av_d:data().
move(_, _, _, #{path := [], state := #{value := State}} = D) ->
  case State of
    walk -> av_d_position:set_state_value(idle, D);
    _    -> D
  end;
move(Dt, MapRect, Blocks, #{id := Id, position := #{value := A}, path := [B|Rest], movement_speed := S, state := #{value := State}} = Data) ->
  case pathfinder_server:next_point(Id, A, B, S, Dt, MapRect, Blocks) of
    undefined ->
      av_d_position:set_path(Rest, Data);
    New ->
      case State of
        idle ->
          NewPlayer = av_d_position:set_position_value(New, Data),
          av_d_position:set_state_value(walk, NewPlayer);
        _ ->
          av_d_position:set_position_value(New, Data)
      end
  end.
