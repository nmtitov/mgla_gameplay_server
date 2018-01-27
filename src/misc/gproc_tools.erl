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

%% API
-export([call/2, cast/2]).

call(Name, Message) ->
  case gproc:where(Name) of
    Pid when is_pid(Pid) -> {ok, gen_server:call(Pid, Message)};
    _                    -> {error, undefined}
  end.

cast(Name, Message) ->
  case gproc:where(Name) of
    Pid when is_pid(Pid) -> gen_server:cast(Pid, Message);
    _                    -> {error, undefined}
  end.
