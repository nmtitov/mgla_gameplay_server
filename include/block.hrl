%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright (C) 2018, N. M. Titov
%%% @doc
%%%
%%% @end
%%% Created : 17. Jan 2018 01:10
%%%-------------------------------------------------------------------
-author("nt").

-record(block, {
  rect :: rect:rect(),
  graph_vertices :: [point:point()]
}).

-type block() :: #block{}.
-export_type([block/0]).
