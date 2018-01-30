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

-spec sort([T]) -> [T].
sort([])    -> [];
sort([H|T]) -> sort([X || X <- T, X < H]) ++ [H] ++ sort([X || X <- T, H < X]).
