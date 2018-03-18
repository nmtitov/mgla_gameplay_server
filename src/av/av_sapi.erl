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

  get_position/1,

  add_event/2,

  update/4,
  broadcast_update/1,

  clear_update_flags/1
]).


handle_click(Id, Point, AvatarId) ->
  gproc_tools:cast(avatar_server:name(Id), {handle_click, Point, AvatarId}).


get_data(Id) ->
  gproc_tools:call(avatar_server:name(Id), get_data).


get_position(Id) ->
  gproc_tools:call(avatar_server:name(Id), get_position).


add_event(E, Id) ->
  gproc_tools:call(avatar_server:name(Id), {add_event,E}).


update(Dt, MapRect, Blocks, Id) ->
  gproc_tools:call(avatar_server:name(Id), {update, Dt, MapRect, Blocks}).

broadcast_update(Id) ->
  gproc_tools:call(avatar_server:name(Id), broadcast_update).

clear_update_flags(Id) ->
  gproc_tools:call(avatar_server:name(Id), clear_update_flags).


-spec handle_click(Id :: id_server:id(), Point :: point:point(), AvatarId :: id_server:id()) -> ok | gproc_tools:not_found().

-spec get_data(Id :: id_server:id()) -> {ok, av:data()} | gproc_tools:not_found().

-spec get_position(Id :: id_server:id()) -> {ok, point:point()} | gproc_tools:not_found().

-spec add_event(GameEvent :: any(), Id :: id_server:id()) -> {ok, _} | gproc_tools:not_found().

-spec update(Dt :: float(), MapRect :: rect:rect(), Blocks :: [block()], Id :: id_server:id()) -> {ok, _} | gproc_tools:not_found().
-spec broadcast_update(Id :: id_server:id()) -> {ok, _} | gproc_tools:not_found().
-spec clear_update_flags(Id :: id_server:id()) -> {ok, av:data()} | gproc_tools:not_found().
