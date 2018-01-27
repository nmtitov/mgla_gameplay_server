%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 17:47
%%%-------------------------------------------------------------------
-module(bot_server).
-author("nt").

-behaviour(gen_server).

-export([start_link/1, get_state/1, set_state/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% gproc

name(Id) -> {n, l, {bot_server, Id}}.

%% API

start_link(Id) ->
  gen_server:start_link(?MODULE, [Id], []).

get_state(Id) ->
  {ok, State} = gproc_tools:call(name(Id), get_state),
  State.

set_state(Id, State) ->
  ok = gproc_tools:cast(name(Id), {set_state, State}).

%% Callback

init([Id]) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  gproc:reg(name(Id)),
  R = {{0, 0}, {600, 1000}},
  Blocks = map_tools:blocks(),
  Position = pathfinder_server:initial_point(Id, R, Blocks),
  Name = <<"Name">>,
  State = avatar:new(Id, bot, Name, Position),
  {ok, State, 0}.

handle_call(get_state, _From, State) ->
  {reply, State, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({set_state, NewState}, _) ->
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  lager:info("~p:~p/~p(timeout, State)", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  Id = avatar:get_id(State),
  map_server:add_avatar(bot, Id),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  lager:info("~p:~p/~p(~p=Reason, State)", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Reason]),
  Id = avatar:get_id(State),
  map_server:remove_avatar(Id),
  gproc:unreg(name(Id)),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
