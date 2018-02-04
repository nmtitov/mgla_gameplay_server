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

-spec init(D :: av:data()) -> jsx:json_text().
init(D) ->
  Id = av:get_id(D),
  Name = av:get_name(D),
  {X, Y} = av_position:get_position_value(D),
  HealthPercent = av_health:get_health_percent(D),
  ManaPercent = av_mana:get_mana_percent(D),
  State = av_position:get_state_value(D),
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

-spec update_message(D :: av:data()) -> jsx:json_text().
update_message(D) ->
  Id = av:get_id(D),
  jsx:encode(#{
    type => update,
    body => #{
      id => Id,
      position => valueOrNull(point:pointToMap(av_position:get_position_value(D)), av_position:get_position_update(D)),
      state => valueOrNull(av_position:get_state_value(D), av_position:get_state_update(D)),
      health_percent => valueOrNull(av_health:get_health_percent(D), av_health:get_health_update(D)),
      mana_percent => valueOrNull(av_mana:get_mana_percent(D), av_mana:get_mana_update(D))
    }
  }).

-spec valueOrNull(X, Update :: boolean()) -> X | null when X :: any().
valueOrNull(Value, true) -> Value;
valueOrNull(_, _) -> null.
