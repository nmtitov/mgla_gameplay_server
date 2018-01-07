-module(id_server).
-author("nt").

-behaviour(gen_server).

-export([start_link/0, id/0]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

id() ->
  gen_server:call(?SERVER, id).

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(init(Args :: term()) ->
  {ok, State :: integer()} | {ok, State :: integer(), timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, 0}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: integer()) ->
  {reply, Reply :: term(), NewState :: integer()} |
  {reply, Reply :: term(), NewState :: integer(), timeout() | hibernate} |
  {noreply, NewState :: integer()} |
  {noreply, NewState :: integer(), timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: integer()} |
  {stop, Reason :: term(), NewState :: integer()}).
handle_call(id, _From, State) ->
  NewState = State + 1,
  {reply, {ok, State}, NewState};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: integer()) ->
  {noreply, NewState :: integer()} |
  {noreply, NewState :: integer(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: integer()}).
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: integer()) ->
  {noreply, NewState :: integer()} |
  {noreply, NewState :: integer(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: integer()}).
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: integer()) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: integer(),
    Extra :: term()) ->
  {ok, NewState :: integer()} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
