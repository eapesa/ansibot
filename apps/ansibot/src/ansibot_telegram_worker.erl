-module(ansibot_telegram_worker).

-behaviour(gen_server).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-export([
  start_link/0,
  reply/3
]).

-record(state, {
  token         = <<"">> :: binary(),
  bot_url       = <<"">> :: binary(),
  poll_interval = 10000 :: integer(),
  update_id     = <<"">> :: binary()
}).

reply(Conn, Method, Opts) ->
  gen_server:cast(Conn, {Method, Opts}).

start_link() ->
  Token     = ansibot_env:get(telegram_token, "abcde12345"),
  BaseUrl   = ansibot_env:get(telegram_bot_api, 
    "https://api.telegram.org/bot"),
  PollTime  = ansibot_env:get(poll_interval, 5000),
  Opts      = #{
    token         => Token,
    bot_url       => BaseUrl,
    poll_interval => PollTime
  },
  gen_server:start_link(?MODULE, [Opts], []).

init([#{ token := Token, bot_url := BotUrl, poll_interval := PollTime }]) ->
  {ok, #state{ token=Token, bot_url=BotUrl, poll_interval=PollTime }, 0}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(get_updates, #state{ token=Token, bot_url=Url, update_id=Uid, 
      poll_interval=PollTime } = State) ->
  NewUrl = Url ++ Token ++ "/getUpdates",
  Qs = case Uid of
    <<>> -> #{};
    Uid  -> #{ <<"offset">> => (Uid + 1) }
  end,
  Response = util_http:get(NewUrl, #{}, Qs),
  case Response of
    #{ status := 200, body := Body } ->
      TupleBody = jsx:decode(Body, [return_maps]),
      Messages = maps:get(<<"result">>, TupleBody, []),
      lists:foreach(
        fun(M) ->
          gen_server:cast(self(), {process, M})
        end,
      Messages);
    Error ->
      io:format("[get_updates] Error getting updates: ~p~n", [Error])
  end,
  erlang:send_after(PollTime, self(), timeout),
  {noreply, State};

handle_cast({process, #{ <<"message">> := #{ <<"chat">> := Chat, <<"text">> := Text },
      <<"update_id">> := Uid } = _M}, State) ->
  Opts = #{
    chat_id => maps:get(<<"id">>, Chat, 1),
    name    => #{ 
      first => maps:get(<<"first_name">>, Chat, <<"Conan">>),
      last  => maps:get(<<"last_name">>, Chat, <<"Edogawa">>)
    },
    text    => Text
  },
  case get_command(Text) of
    false ->
      NewOpts = maps:put(text, <<"I am so sorry but I am not programmed to cater to ",
          "that message.">>, Opts),
      gen_server:cast(self(), {send_message, NewOpts});
    {ok, Op, Params} -> 
      spawn(ansibot_telegram_ops, process, [self(), Op, Params, Opts])
  end,
  {noreply, State#state{ update_id=Uid }};

handle_cast({send_message, #{ chat_id := ChatId, text := Text}}, #state{ token=Token,
    bot_url=Url } = State) ->
  NewUrl = Url ++ Token ++ "/sendMessage",
  Response = util_http:get(NewUrl, #{}, #{ <<"chat_id">> => ChatId, <<"text">> => Text,
    <<"parse_mode">> => <<"markdown">> }),
  % io:format("[send_message] Response: ~p~n", [Response]),
  {noreply, State};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  gen_server:cast(self(), get_updates),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===============%%%
%%% EXPORTED APIS %%%
%%%===============%%%


%%%===============%%%
%%% LIBRARIES     %%%
%%%===============%%%
get_command(<<"/start">>) ->
  {ok, <<"start">>, <<>>};
get_command(<<"/help">>) ->
  {ok, <<"help">>, <<>>};
get_command(<<"/help ", Command/binary>>) -> 
  {ok, <<"help">>, Command};
get_command(<<"/release_notes ", Params/binary>>) ->
  {ok, <<"release_notes">>, get_command_params(Params)};
get_command(<<"/tag ", Params/binary>>) ->
  {ok, <<"tag">>, get_command_params(Params)};
get_command(<<"/build ", Params/binary>>) ->
  {ok, <<"build">>, get_command_params(Params)};
get_command(<<"/file ", Params/binary>>) ->
  {ok, <<"file">>, get_command_params(Params)};
get_command(_) -> false.

get_command_params(Params) ->
  Split = binary:split(Params, [<<" ">>], [global]),
  get_command_params(Split, maps:new()).

get_command_params([], Acc) -> Acc;
get_command_params([<<"@", Key/binary>>, Value | T], Acc) ->
  get_command_params(T, maps:put(Key, Value, Acc));
get_command_params([Value], Acc) ->
  get_command_params([], maps:put(text, Value, Acc));
get_command_params(Params, Acc) -> 
  get_command_params([], maps:put(text, Params, Acc)).

% http://stackoverflow.com/questions/31078710/how-to-obtain-telegram-chat-id-for-a-specific-user