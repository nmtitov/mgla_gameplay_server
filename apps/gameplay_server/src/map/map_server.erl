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
    players :: [players:player()],
    blocks :: [map_tools:block()]
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
  MapRect = {{0, 0}, {600, 1000}},
  Players = players:players(),
  Blocks = map_tools:blocks(),
  State = #map_server_state{map_rect = MapRect, players = Players, blocks = Blocks},
  {ok, State, 0}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({enter, Id}, #map_server_state{players = Players} = MapServerState) ->
  NewPlayers = players:add(Players, Id),
  ws_send:send_map(Id),
  ws_send:broadcast_enter(Id),
  NewState = MapServerState#map_server_state{players = NewPlayers},
  {noreply, NewState};
handle_cast({leave, Id}, #map_server_state{players = Players} = MapServerState) ->
  NewPlayers = players:remove(Players, Id),
  ws_send:broadcast_leave(Id),
  NewState = MapServerState#map_server_state{players = NewPlayers},
  {noreply, NewState};
handle_cast({input, Id, T}, #map_server_state{players = Players, blocks = Blocks} = MapServerState) ->
  NewPlayers = players:handle_input(Players, Id, T, Blocks),
  NewState = MapServerState#map_server_state{players = NewPlayers},
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({timeout, _Ref, tick}, #map_server_state{map_rect = MapRect, players = Players, blocks = Blocks} = MapServerState) ->
  TimeA = erlang:system_time(),
  NewPlayers = players:update(Players, ?TICK_RATE / 1000.0, MapRect, Blocks),
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
