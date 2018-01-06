-module(response).
-author("nt").

-export([teleport/1]).

-type teleport() :: binary().
-export_type([teleport/0]).

-spec teleport(point:point()) -> teleport().
teleport(P) ->
  {X, Y} = P,
  jsx:encode(#{
    x => X,
    y => Y
  }).