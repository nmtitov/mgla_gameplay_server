-module(size).
-author("nt").

-export([size/2, width/1, height/1]).

-type size() :: {number(), number()}.
-export_type([size/0]).

-spec size(number(), number()) -> size().
size(W, H) -> {float(W), float(H)}.

-spec width(size()) -> float().
width({W, _}) -> W.

-spec height(size()) -> float().
height({_, H}) -> H.
