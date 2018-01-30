%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 00:32
%%%-------------------------------------------------------------------
-module(avatar_sapi).
-author("nt").

%% API
-export([
  handle_click/2,

  get_data/1,
  set_data/2,

  get_position/1,
  set_position/2,

  add_health/2,
  subtract_health/2,

  add_mana/2,
  subtract_mana/2,

  get_state/1,
  set_state/2
]).

-spec handle_click(Id :: id_server:id(), Point :: point:point()) -> ok | gproc_tools:not_found().
handle_click(Id, Point) ->
  gproc_tools:cast(av_s:name(Id), {handle_click, Point}).

-spec get_data(Id :: id_server:id()) -> {ok, av_d:data()} | gproc_tools:not_found().
get_data(Id) ->
  gproc_tools:call(av_s:name(Id), get_data).

-spec set_data(Data :: av_d:data(), Id :: id_server:id()) -> ok | gproc_tools:not_found().
set_data(Data, Id) ->
  gproc_tools:cast(av_s:name(Id), {set_data, Data}).

-spec get_position(Id :: id_server:id()) -> {ok, point:point()} | gproc_tools:not_found().
get_position(Id) ->
  gproc_tools:call(av_s:name(Id), get_position).

-spec set_position(P :: point:point(), Id :: id_server:id()) -> ok | gproc_tools:not_found().
set_position(P, Id) ->
  gproc_tools:cast(av_s:name(Id), {set_position, P}).

-spec add_health(X :: number(), Id :: id_server:id()) -> ok | gproc_tools:not_found().
add_health(X, Id) ->
  gproc_tools:cast(av_s:name(Id), {add_health, X}).

-spec subtract_health(X :: number(), Id :: id_server:id()) -> ok | gproc_tools:not_found().
subtract_health(X, Id) ->
  gproc_tools:cast(av_s:name(Id), {subtract_health, X}).

-spec add_mana(X :: number(), Id :: id_server:id()) -> ok | gproc_tools:not_found().
add_mana(X, Id) ->
  gproc_tools:cast(av_s:name(Id), {add_mana, X}).

-spec subtract_mana(X :: number(), Id :: id_server:id()) -> ok | gproc_tools:not_found().
subtract_mana(X, Id) ->
  gproc_tools:cast(av_s:name(Id), {subtract_mana, X}).

-spec get_state(Id :: id_server:id()) -> {ok, av_d:state()} | gproc_tools:not_found().
get_state(Id) ->
  gproc_tools:call(av_s:name(Id), get_state).

-spec set_state(X :: av_d:state(), Id :: id_server:id()) -> ok | gproc_tools:not_found().
set_state(X, Id) ->
  gproc_tools:cast(av_s:name(Id), {set_state, X}).