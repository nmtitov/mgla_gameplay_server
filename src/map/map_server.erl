-module(map_server).
-author("nt").
-behaviour(gen_server).
-include("../../include/block.hrl").

-define(UPDATE_RATE, 33).

-export([start_link/0, add_avatar/2, remove_avatar/1, get_avatars_meta/0]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-spec add_avatar(Type :: atom(), Id :: id_server:id()) -> ok.
add_avatar(Type, Id) ->
  gen_server:cast(?SERVER, {add_avatar, Type, Id}).

-spec remove_avatar(Id :: id_server:id()) -> ok.
remove_avatar(Id) ->
  gen_server:cast(?SERVER, {remove_avatar, Id}).

-spec get_avatars_meta() -> [data_avatar:data()].
get_avatars_meta() ->
  gen_server:call(?SERVER, get_avatars_meta).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Callbacks

init(Params) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, Params]),
  Blocks = map_tools:blocks(),
  State = #{
    rect => {{0, 0}, {600, 1000}},
    avatars => [],
    blocks => Blocks
  },
  {ok, State, 0}.

handle_call(get_avatars_meta, _From, #{avatars := AvatarsMeta} = State) ->
  {reply, AvatarsMeta, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({add_avatar, Type, Id}, #{avatars := AvatarsMeta} = State) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),

  ws_handler:broadcast(ws_send:enter_message(Id)),

  {ok, Data} = avatar_server:get_data(Id),
  case Type of
    player ->
      ws_handler:send(Id, map_tools:map()),
      ws_handler:send(Id, ws_send:id(Id)),
      ws_handler:send(Id, ws_send:init(Data)),
      lists:foreach(fun({_, Id_}) ->
        {ok, Data_} = avatar_server:get_data(Id_),
        ws_handler:send(Id, ws_send:init(Data_))
      end, AvatarsMeta);
    _ -> ok
  end,

  lists:foreach(fun({_, Id_}) ->
    ws_handler:send(Id_, ws_send:init(Data))
  end, lists:filter(fun({Type_, _}) ->
    Type_ == player
  end, AvatarsMeta)),

  NewState = State#{avatars := [{Type, Id}|AvatarsMeta]},
  {noreply, NewState};

handle_cast({remove_avatar, Id}, #{avatars := AvatarsMeta} = State) ->
  lager:info("~p:~p/~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  NewAvatarsMeta = lists:filter(fun({_, Id2}) -> Id =/= Id2 end, AvatarsMeta),
  NewState = State#{avatars := NewAvatarsMeta},
  ws_handler:broadcast(ws_send:leave_message(Id)),
  {noreply, NewState};

handle_cast(_Request, State) ->
  {noreply, State}.


handle_info({timeout, _Ref, update}, State) ->
  {noreply, update(State)};

handle_info(timeout, State) ->
  lager:info("~p:~p/~p(~p, State)", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, timeout]),
  bot_factory_sup:start_bots(),
  NewState = update(State),
  {noreply, NewState};

handle_info(Info, State) ->
  lager:info("~p:~p/~p(~p = Info, State)", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Info]),
  {noreply, State}.


terminate(_Reason = M, _State) ->
  lager:info("~p:~p(~p)", [?MODULE, ?FUNCTION_NAME, M]),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Private

update(#{rect := MapRect, avatars := AvatarsMeta, blocks := Blocks} = State) ->
  TimeA = erlang:system_time(),
  Dt = ?UPDATE_RATE / 1000.0,

  do_update(AvatarsMeta, Dt, MapRect, Blocks),

  TimeB = erlang:system_time(),
  _ = TimeB - TimeA,
%%  lager:info("TimeDelta=~p", [TimeDelta]),
  erlang:start_timer(?UPDATE_RATE, self(), update),
  State.

do_update(AvatarsMeta, Dt, MapRect, Blocks) ->
  lists:foreach(fun({_, Id}) ->
    avatar_server:update(Dt, MapRect, Blocks, Id)
  end, AvatarsMeta),

  lists:foreach(fun({_, Id}) ->
    avatar_server:broadcast_update(Id)
  end, AvatarsMeta),

  lists:foreach(fun({_, Id}) ->
    {ok, _} = avatar_server:clear_update_flags(Id)
  end, AvatarsMeta).
