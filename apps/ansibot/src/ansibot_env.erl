-module(ansibot_env).

-define(APPLICATION, ansibot).

-export([ get/2, get/3, set/2 ]).

get(Key, Default) ->
  case application:get_env(?APPLICATION, Key) of
    {ok, Value} ->
      Value;
    undefined ->
      Default
  end.

get(Application, Key, Default) ->
  case application:get_env(Application, Key) of
    {ok, Value} ->
      Value;
    undefined ->
      Default
  end.

set(Key, Value) ->
  application:set_env(?APPLICATION, Key, Value).
