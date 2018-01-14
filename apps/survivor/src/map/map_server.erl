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

-record(map_server_state, {
    map_rect :: rect:rect(),
    players :: [players:player()]
}).

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
  MapRect = rect:rect(point:point(0, 0), size:size(600, 1000)),
  Players = players:players(),
  State = #map_server_state{map_rect = MapRect, players = Players},
  {ok, State, 0}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({enter, Id}, #map_server_state{players = Players} = MapServerState) ->
  NewPlayers = players:add(Players, Id),
  ws_send:map(Id),
  ws_send:enter(Id),
  NewState = MapServerState#map_server_state{players = NewPlayers},
  {noreply, NewState};
handle_cast({leave, Id}, #map_server_state{players = Players} = MapServerState) ->
  NewPlayers = players:remove(Players, Id),
  ws_send:leave(Id),
  NewState = MapServerState#map_server_state{players = NewPlayers},
  {noreply, NewState};
handle_cast({input, Id, T}, #map_server_state{players = Players} = MapServerState) ->
  NewPlayers = players:input(Players, Id, T),
  NewState = MapServerState#map_server_state{players = NewPlayers},
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({timeout, _Ref, tick}, #map_server_state{map_rect = MapRect, players = Players} = MapServerState) ->
  TimeA = erlang:system_time(),
  NewPlayers = players:update(Players, ?TICK_RATE / 1000.0, MapRect),
  NewState = MapServerState#map_server_state{players = NewPlayers},
  TimeB = erlang:system_time(),
  _ = TimeB - TimeA,
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
