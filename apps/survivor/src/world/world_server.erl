-module(world_server).
-author("nt").

-behaviour(gen_server).

-define(TICK_RATE, 33).

-export([start_link/0, connect/1, disconnect/1, target/2]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  id = 1,
  webSocket = undefined,
  current = undefined,
  target = undefined
}).

connect(WebSocket) ->
  gen_server:cast(?SERVER, {connect, WebSocket}).

disconnect(WebSocket) ->
  gen_server:cast(?SERVER, {disconnect, WebSocket}).

target(WebSocket, P) ->
  gen_server:cast(?SERVER, {target, P}).

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, [], 0}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({connect, Websocket}, State) ->
  {noreply, [#state{webSocket = Websocket, current = point:point(0, 0)}]};
handle_cast({disconnect, Websocket}, State) ->
  {noreply, []};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({timeout, _Ref, tick}, State) ->
  lists:foreach(fun(State2) ->
    WebSocket = State2#state.webSocket,
    P = State2#state.current,
    websocket_handler:teleport(WebSocket, P)
  end, State),
  schedule_next_tick(),
  {noreply, State};

handle_info(timeout, State) ->
  schedule_next_tick(),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

schedule_next_tick() ->
  erlang:start_timer(?TICK_RATE, self(), tick).

%%  Reply = case T of
%%     undefined ->
%%       {ok, State};
%%     T ->
%%       case movement:next_point(C, T, ?TICK_RATE / 1000.0, 100.0) of
%%         undefined ->
%%           {ok, State#state{target = undefined}};
%%         New ->
%%           Message = response:teleport(New),
%%           {reply, {text, Message}, State#state{current = New}}
%%       end
%%  end,
