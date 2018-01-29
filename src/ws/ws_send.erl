-module(ws_send).
-author("nt").
-export([id/1, init/1, deinit/1, enter_message/1, leave_message/1, update_message/3]).

-spec id(Id::id_server:id()) -> jsx:json_text().
id(Id) ->
  jsx:encode(#{
    type => id,
    body => #{
      id => Id
    }
  }).

-spec init(Avatar) -> Message when Avatar :: avatar_data:data(), Message :: jsx:json_text().
init(A) ->
  Id = avatar_data:get_id(A),
  Name = avatar_data:get_name(A),
  {X, Y} = avatar_data:get_position_value(A),
  HealthPercent = avatar_data:get_health_percent(A),
  ManaPercent = avatar_data:get_mana_percent(A),
  State = avatar_data:get_state_value(A),
  jsx:encode(#{
    type => init,
    body => #{
      id => Id,
      name => Name,
      position => #{
        x => X,
        y => Y
      },
      health_percent => HealthPercent,
      mana_percent => ManaPercent,
      state => State
    }
  }).

-spec deinit(Id) -> Message when Id :: id_server:id(), Message :: jsx:json_text().
deinit(Id) ->
  jsx:encode(#{
    type => deinit,
    body => #{
      id => Id
    }
  }).

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

-spec update_message(Id, Point, State) -> Message when Id :: id_server:id(), Point :: point:point(), State :: avatar_data:state(), Message :: jsx:json_text().
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
