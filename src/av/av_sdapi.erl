%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Feb 2018 11:25
%%%-------------------------------------------------------------------
-module(av_sdapi).
-author("nt").
-include("../../include/block.hrl").

%% API
-export([
  set_position/2,

  get_state/1,
  set_state/2,

  move/4,

  is_dirty/1
]).


-spec set_position(P :: point:point(), Id :: id_server:id()) -> ok | gproc_tools:not_found().
set_position(P, Id) ->
  gproc_tools:cast(av_srv:name(Id), {set_position, P}).


-spec get_state(Id :: id_server:id()) -> {ok, av:state()} | gproc_tools:not_found().
get_state(Id) ->
  gproc_tools:call(av_srv:name(Id), get_state).

-spec set_state(X :: av:state(), Id :: id_server:id()) -> ok | gproc_tools:not_found().
set_state(X, Id) ->
  gproc_tools:cast(av_srv:name(Id), {set_state, X}).


-spec move(Dt :: float(), MapRect :: rect:rect(), Blocks :: [block()], Id :: id_server:id()) -> {ok, av:data()} | gproc_tools:not_found().
move(Dt, MapRect, Blocks, Id) ->
  gproc_tools:call(av_srv:name(Id), {move, Dt, MapRect, Blocks}).


-spec is_dirty(Id :: id_server:id()) -> {ok, boolean()} | gproc_tools:not_found().
is_dirty(Id) ->
  gproc_tools:call(av_srv:name(Id), is_dirty).
