-module(ws_send).
-author("nt").
-include("../../include/avatar.hrl").
-export([update_message/3, enter_message/1, leave_message/1]).

-spec update_message(Id, Point, State) -> Message when Id :: id_server:id(), Point :: point:point(), State :: avatar_state(), Message :: map().
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

enter_message(Id) ->
  jsx:encode(#{
    type => enter,
    body => #{
      id => Id
    }
  }).

leave_message(Id) ->
  jsx:encode(#{
    type => leave,
    body => #{
      id => Id
    }
  }).