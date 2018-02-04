%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 00:32
%%%-------------------------------------------------------------------
-module(av_sapi).
-author("nt").
-include("../../include/block.hrl").

%% API
-export([
  handle_click/3,

  get_data/1,

  add_health/2,
  subtract_health/2,

  add_mana/2,
  subtract_mana/2,

  update/4,
  broadcast_update/1,

  clear_update_flags/1
]).

handle_click(Id, Point, AvatarId) ->
  gproc_tools:cast(av_srv:name(Id), {handle_click, Point, AvatarId}).


get_data(Id) ->
  gproc_tools:call(av_srv:name(Id), get_data).


add_health(X, Id) ->
  gproc_tools:cast(av_srv:name(Id), {add_health, X}).

subtract_health(X, Id) ->
  gproc_tools:call(av_srv:name(Id), {subtract_health, X}).


add_mana(X, Id) ->
  gproc_tools:cast(av_srv:name(Id), {add_mana, X}).

subtract_mana(X, Id) ->
  gproc_tools:cast(av_srv:name(Id), {subtract_mana, X}).


update(Dt, MapRect, Blocks, Id) ->
  gproc_tools:call(av_srv:name(Id), {update, Dt, MapRect, Blocks}).

broadcast_update(Id) ->
  gproc_tools:call(av_srv:name(Id), broadcast_update).

clear_update_flags(Id) ->
  gproc_tools:call(av_srv:name(Id), clear_update_flags).


-spec handle_click(Id :: id_server:id(), Point :: point:point(), AvatarId :: id_server:id()) -> ok | gproc_tools:not_found().

-spec get_data(Id :: id_server:id()) -> {ok, av:data()} | gproc_tools:not_found().

-spec add_health(X :: number(), Id :: id_server:id()) -> ok | gproc_tools:not_found().
-spec subtract_health(X :: number(), Id :: id_server:id()) -> {ok, av:data()} | gproc_tools:not_found().

-spec add_mana(X :: number(), Id :: id_server:id()) -> ok | gproc_tools:not_found().
-spec subtract_mana(X :: number(), Id :: id_server:id()) -> ok | gproc_tools:not_found().

-spec update(Dt :: float(), MapRect :: rect:rect(), Blocks :: [block()], Id :: id_server:id()) -> {ok, _} | gproc_tools:not_found().
-spec broadcast_update(Id :: id_server:id()) -> {ok, _} | gproc_tools:not_found().
-spec clear_update_flags(Id :: id_server:id()) -> {ok, av:data()} | gproc_tools:not_found().
