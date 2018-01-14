%%%-------------------------------------------------------------------
%%% @author Nikita Titov
%%% @copyright © 2018 N. M. Titov. All rights reserved.
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2018 13:27
%%%-------------------------------------------------------------------
-module(rect).
-author("nt").

%% API
-export([rect/2, contains/2]).

-type rect() :: {{float(), float()}, {float(), float()}}.
-export_type([rect/0]).

-spec rect(Origin, Size) -> Rect when Origin :: point:point(), Size :: size:size(), Rect :: rect().
rect(Origin, Size) -> {Origin, Size}.

-spec contains(Rect, Point) -> boolean() when Rect :: rect(), Point :: point:point().
contains({{OriginX, OriginY}, {W, H}}, {X, Y}) -> ((OriginX < X) and (X < (OriginX + W))) and ((OriginY < Y) and (Y < (OriginY + H))).
