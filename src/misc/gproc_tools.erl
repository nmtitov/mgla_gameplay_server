%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jan 2018 17:26
%%%-------------------------------------------------------------------
-module(gproc_tools).
-author("nt").

-type result() :: {ok, term()}.
-type not_found() :: {not_found, gproc:key()}.
-export_type([result/0, not_found/0]).

%% API
-export([call/2, cast/2]).

-spec call(Key :: term(), Message :: term()) -> result() | not_found().
call(Key, Message) ->
  case gproc:where(Key) of
    Pid when is_pid(Pid) -> {ok, gen_server:call(Pid, Message)};
    _                    -> {not_found, Key}
  end.

-spec cast(Key :: term(), Message :: term()) -> ok | not_found().
cast(Key, Message) ->
  case gproc:where(Key) of
    Pid when is_pid(Pid) -> gen_server:cast(Pid, Message);
    _                    -> {not_found, Key}
  end.
