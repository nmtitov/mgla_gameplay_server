%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2018 18:49
%%%-------------------------------------------------------------------
-module(id_server).
-author("nt").

-behaviour(gen_server).

-export([start_link/0, get_id/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type id() :: non_neg_integer().
-export_type([id/0]).

-spec get_id() -> Id when Id :: id().
get_id() ->
  gen_server:call(?SERVER, id).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, 0}.

handle_call(id, _From, State) ->
  NewState = State + 1,
  {reply, State, NewState};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
