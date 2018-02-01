%%%-------------------------------------------------------------------
%%% @author nt
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2018 20:39
%%%-------------------------------------------------------------------
-module(av_misc).
-author("nt").

%% API
-export([is_valid_target/2]).

-spec is_valid_target(AttackerId :: id_server:id(), TargetId :: id_server:id() | undefined) -> boolean().
is_valid_target(Id, Id)       -> false;
is_valid_target(_, undefined) -> false;
is_valid_target(_, _)         -> true.
