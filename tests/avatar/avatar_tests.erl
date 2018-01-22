%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 13:03
%%%-------------------------------------------------------------------
-module(avatar_tests).
-author("nt").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

new_test() ->
  Id = 0,
  Position = 0,
  Data = avatar:new(Id, Position),
  #{id := Id} = Data.
