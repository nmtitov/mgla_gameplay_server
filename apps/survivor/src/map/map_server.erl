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

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(init(Args :: term()) ->
  {ok, State :: [map_state:player()]} | {ok, State :: [map_state:player()], timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, [], 0}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: [map_state:player()]) ->
  {reply, Reply :: term(), NewState :: [map_state:player()]} |
  {reply, Reply :: term(), NewState :: [map_state:player()], timeout() | hibernate} |
  {noreply, NewState :: [map_state:player()]} |
  {noreply, NewState :: [map_state:player()], timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: [map_state:player()]} |
  {stop, Reason :: term(), NewState :: [map_state:player()]}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: [map_state:player()]) ->
  {noreply, NewState :: [map_state:player()]} |
  {noreply, NewState :: [map_state:player()], timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: [map_state:player()]}).
handle_cast({enter, Id}, State) ->
  NewState = players:add(State, Id),
  {noreply, NewState};
handle_cast({leave, Id}, State) ->
  NewState = players:remove(State, Id),
  {noreply, NewState};
handle_cast({input, Id, T}, State) ->
  NewState = players:input(State, Id, T),
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: [map_state:player()]) ->
  {noreply, NewState :: [map_state:player()]} |
  {noreply, NewState :: [map_state:player()], timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: [map_state:player()]}).
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

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: [map_state:player()]) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: [map_state:player()],
    Extra :: term()) ->
  {ok, NewState :: [map_state:player()]} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

schedule_next_tick() ->
  erlang:start_timer(?TICK_RATE, self(), tick).
