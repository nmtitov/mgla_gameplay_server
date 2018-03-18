-module(ws_send).
-author("nt").
-export([id/1, init/1, deinit/1, enter_message/1, leave_message/1, update_message/1]).

-spec id(Id :: id_server:id()) -> jsx:json_text().
id(Id) ->
  jsx:encode(#{
    type => id,
    body => #{
      id => Id
    }
  }).

-spec init(D :: data_avatar:data()) -> jsx:json_text().
init(D) ->
  Id = data_avatar:get_id(D),
  Name = data_avatar:get_name(D),
  {X, Y} = data_position:get_position_value(D),
  HealthPercent = data_health:get_health_percent(D),
  ManaPercent = data_mana:get_mana_percent(D),
  State = data_position:get_state_value(D),
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

-spec deinit(id_server:id()) -> jsx:json_text().
deinit(Id) ->
  jsx:encode(#{
    type => deinit,
    body => #{
      id => Id
    }
  }).

-spec enter_message(Id :: id_server:id()) -> jsx:json_text().
enter_message(Id) ->
  jsx:encode(#{
    type => enter,
    body => #{
      id => Id
    }
  }).

-spec leave_message(Id :: id_server:id()) -> jsx:json_text().
leave_message(Id) ->
  jsx:encode(#{
    type => leave,
    body => #{
      id => Id
    }
  }).

-spec update_message(D :: data_avatar:data()) -> jsx:json_text().
update_message(D) ->
  Id = data_avatar:get_id(D),
  jsx:encode(#{
    type => update,
    body => #{
      id => Id,
      position => valueOrNull(point:pointToMap(data_position:get_position_value(D)), data_position:get_position_update(D)),
      state => valueOrNull(data_position:get_state_value(D), data_position:get_state_update(D)),
      health_percent => valueOrNull(data_health:get_health_percent(D), data_health:get_health_update(D)),
      mana_percent => valueOrNull(data_mana:get_mana_percent(D), data_mana:get_mana_update(D))
    }
  }).

-spec valueOrNull(X, Update :: boolean()) -> X | null when X :: any().
valueOrNull(Value, true) -> Value;
valueOrNull(_, _) -> null.
