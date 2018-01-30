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

-spec handle_click(Id :: id_server:id(), Point :: point:point()) -> ok.
handle_click(Id, Point) ->
  ok = gproc_tools:cast(avatar_server:name(Id), {handle_click, Point}).

-spec get_data(Id :: id_server:id()) -> Data :: avatar_data:data().
get_data(Id) ->
  {ok, Data} = gproc_tools:call(avatar_server:name(Id), get_data),
  Data.

-spec set_data(Data :: avatar_data:data(), Id :: id_server:id()) -> ok.
set_data(Data, Id) ->
  ok = gproc_tools:cast(avatar_server:name(Id), {set_data, Data}).

-spec get_position(Id :: id_server:id()) -> point:point().
get_position(Id) ->
  {ok, Value} = gproc_tools:call(avatar_server:name(Id), get_position),
  Value.

-spec set_position(P :: point:point(), Id :: id_server:id()) -> ok.
set_position(P, Id) ->
  ok = gproc_tools:cast(avatar_server:name(Id), {set_position, P}).

-spec add_health(X :: number(), Id :: id_server:id()) -> ok.
add_health(X, Id) ->
  ok = gproc_tools:cast(avatar_server:name(Id), {add_health, X}).

-spec subtract_health(X :: number(), Id :: id_server:id()) -> ok.
subtract_health(X, Id) ->
  ok = gproc_tools:cast(avatar_server:name(Id), {subtract_health, X}).

-spec add_mana(X :: number(), Id :: id_server:id()) -> ok.
add_mana(X, Id) ->
  ok = gproc_tools:cast(avatar_server:name(Id), {add_mana, X}).

-spec subtract_mana(X :: number(), Id :: id_server:id()) -> ok.
subtract_mana(X, Id) ->
  ok = gproc_tools:cast(avatar_server:name(Id), {subtract_mana, X}).

-spec get_state(Id :: id_server:id()) -> avatar_data:state().
get_state(Id) ->
  {ok, X} = gproc_tools:call(avatar_server:name(Id), get_state),
  X.

-spec set_state(X :: avatar_data:state(), Id :: id_server:id()) -> ok.
set_state(X, Id) ->
  ok = gproc_tools:cast(avatar_server:name(Id), {set_state, X}).