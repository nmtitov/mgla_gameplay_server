-module(response).
-author("nt").

-export([teleport/2]).

-type teleport() :: binary().
-export_type([teleport/0]).

-spec teleport(non_neg_integer(), point:point()) -> teleport().
teleport(Id, P) ->
  {X, Y} = P,
  jsx:encode(#{
    id => Id,
    point => #{
      x => X,
      y => Y
    }
  }).