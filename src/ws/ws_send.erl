-module(ws_send).
-author("nt").
-include("../../include/avatar.hrl").
-export([enter_message/1, leave_message/1, update_message/3]).

-spec enter_message(Id) -> Message when Id :: id_server:id(), Message :: jsx:json_text().
enter_message(Id) ->
  jsx:encode(#{
    type => enter,
    body => #{
      id => Id
    }
  }).

-spec leave_message(Id) -> Message when Id :: id_server:id(), Message :: jsx:json_text().
leave_message(Id) ->
  jsx:encode(#{
    type => leave,
    body => #{
      id => Id
    }
  }).

-spec update_message(Id, Point, State) -> Message when Id :: id_server:id(), Point :: point:point(), State :: avatar_state(), Message :: jsx:json_text().
update_message(Id, {X, Y}, State) ->
  jsx:encode(#{
    type => teleport,
    body => #{
      id => Id,
      point => #{
        x => X,
        y => Y
      },
      new_state => if
                     State == undefined -> null;
                     true -> State
                   end
    }
  }).
