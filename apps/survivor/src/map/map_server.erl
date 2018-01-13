-module(map_server).
-author("nt").

-behaviour(gen_server).

-define(TICK_RATE, 33).

-export([start_link/0, enter/1, leave/1, input/2]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-spec enter(non_neg_integer()) -> ok.
enter(Id) ->
  gen_server:cast(?SERVER, {enter, Id}).

-spec leave(non_neg_integer()) -> ok.
leave(Id) ->
  gen_server:cast(?SERVER, {leave, Id}).

-spec input(non_neg_integer(), point:point()) -> ok.
input(Id, P) ->
  gen_server:cast(?SERVER, {input, Id, P}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Callbacks

init(_) ->
  {ok, players:players(), 0}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({enter, Id}, State) ->
  NewState = players:add(State, Id),
  ws_send:map(Id),
  ws_send:enter(Id),
  {noreply, NewState};
handle_cast({leave, Id}, State) ->
  NewState = players:remove(State, Id),
  ws_send:leave(Id),
  {noreply, NewState};
handle_cast({input, Id, T}, State) ->
  NewState = players:input(State, Id, T),
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({timeout, _Ref, tick}, State) ->
  TimeA = erlang:system_time(),
  NewState = players:update(State, ?TICK_RATE / 1000.0),
  TimeB = erlang:system_time(),
  TimeDiff = TimeB - TimeA,
  schedule_next_tick(),
  {noreply, NewState};
handle_info(timeout, State) ->
  schedule_next_tick(),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private

schedule_next_tick() ->
  erlang:start_timer(?TICK_RATE, self(), tick).
