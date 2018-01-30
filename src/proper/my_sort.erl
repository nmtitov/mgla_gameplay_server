%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 11:10
%%%-------------------------------------------------------------------
-module(my_sort).
-author("nt").

%% API
-export([sort/1]).
%%-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec sort([T]) -> [T].
sort([])    -> [];
sort([H|T]) -> sort([X || X <- T, X < H]) ++ [H] ++ sort([X || X <- T, H < X]).

sort_test_() ->
  [test_zero(), test_two(), test_four()].

test_zero() ->
  ?_assertEqual([], sort([])). % notice underscore
test_two() ->
  [?_assertEqual([17,42], sort([X,Y])) || {X,Y} <- [{17,42}, {42,17}]].
test_four() ->
  [?_assertEqual([1,2,3,4], sort([3,1,4,2]))].


prop_ordered() ->
  ok.