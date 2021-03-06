%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright (C) 2018, N. M. Titov
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 20:39
%%%-------------------------------------------------------------------
-module(avatar_misc).
-author("nt").

%% API
-export([
  is_valid_target/1,
  do_is_valid_target/2,
  is_in_range/3
]).


-spec is_valid_target(Data :: autoattack:data()) -> boolean().
is_valid_target(D) ->
  Id = autoattack:get_id(D),
  TargetId = autoattack:get_target(D),
  do_is_valid_target(Id, TargetId).

-spec do_is_valid_target(AttackerId :: id_server:id(), TargetId :: id_server:id() | undefined) -> boolean().
do_is_valid_target(Id, Id)       -> false;
do_is_valid_target(_, undefined) -> false;
do_is_valid_target(_, _)         -> true.

-spec is_in_range(Range :: number(), Position :: point:point(), TargetId :: id_server:id()) -> boolean().
is_in_range(Range, Position, TargetId) ->
  {ok, TargetPosition} = avatar_server:get_position(TargetId),
  point:distance(Position, TargetPosition) < Range.
