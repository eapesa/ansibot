-module(util_http).

-compile({no_auto_import,[put/2]}).

-export([
  get/1,
  get/2,
  get/3,
  post/1,
  post/2,
  post/3,
  post/4,
  put/1,
  put/2,
  put/3,
  put/4,
  delete/1,
  delete/2,
  delete/3
]).


get(#{url := Url, headers := Headers, qs := Qs}) ->
  get(Url, Headers, Qs);

get(#{url := Url, headers := Headers}) ->
  get(Url, Headers);

get(#{url := Url}) ->
  get(Url, #{}); %% had to specify the second parameter because of BIF get/1

get(Url) ->
  get(Url, #{}).

get(Url, Headers) ->
  get(Url, Headers, #{}).

get(Url, Headers, Qs) ->
  request(get, Url, Headers, Qs, []).

post(#{url := Url, headers := Headers, qs := Qs, body := Body}) ->
  post(Url, Headers, Qs, Body);

post(#{url := Url, headers := Headers, qs := Qs}) ->
  post(Url, Headers, Qs);

post(#{url := Url, headers := Headers}) ->
  post(Url, Headers);

post(#{url := Url}) ->
  post(Url);

post(Url) ->
  post(Url, #{}).

post(Url, Headers) ->
  post(Url, Headers, #{}).

post(Url, Headers, Qs) ->
  request(post, Url, Headers, Qs, []).

post(Url, Headers, Qs, Body) ->
  request(post, Url, Headers, Qs, Body).

put(#{url := Url, headers := Headers, qs := Qs, body := Body}) ->
  put(Url, Headers, Qs, Body);

put(#{url := Url, headers := Headers, qs := Qs}) ->
  put(Url, Headers, Qs);

put(#{url := Url, headers := Headers}) ->
  put(Url, Headers);

put(#{url := Url}) ->
  put(Url);

put(Url) ->
  put(Url, #{}).

put(Url, Headers) ->
  put(Url, Headers, #{}).

put(Url, Headers, Qs) ->
  request(put, Url, Headers, Qs, []).

put(Url, Headers, Qs, Body) ->
  request(put, Url, Headers, Qs, Body).

delete(#{url := Url, headers := Headers, qs := Qs}) ->
  delete(Url, Headers, Qs);

delete(#{url := Url, headers := Headers}) ->
  delete(Url, Headers);

delete(#{url := Url}) ->
  delete(Url, #{}); %% had to specify the second parameter because of BIF delete/1

delete(Url) ->
  delete(Url, #{}).

delete(Url, Headers) ->
  delete(Url, Headers, #{}).

delete(Url, Headers, Qs) ->
  request(delete, Url, Headers, Qs, []).

delete(Url, Headers, Qs, Body) ->
  request(delete, Url, Headers, Qs, Body).

request(Method, TmpUrl, TmpHeaders, Queries, Body) ->
  Url = TmpUrl ++ qs(Queries),
  Headers = maps:to_list(TmpHeaders),
  case ibrowse:send_req(Url, Headers, Method, Body) of
    {ok, StatusCode, ResHeaders, ResBody} ->
      #{
        status  => list_to_integer(StatusCode),
        headers => maps:from_list(ResHeaders),
        body    => list_to_binary(ResBody)
      };
    Error = {error, _Err} ->
      Error
  end.

qs(Map) ->
  qs(from_map, maps:to_list(Map)).

qs(from_map, [Head|Tail]) ->
  "?" ++ [make_query(Head) | [["&", make_query(Elem)] || Elem <- Tail]];
qs(from_map, []) -> [].

make_query({Key, Value}) when Value =:= true orelse Value =:= false ->
  make_query({Key, atom_to_list(Value)});

make_query({Key, Value}) when is_binary(Value) ->
  make_query({Key, binary_to_list(Value)});

make_query({Key, Value}) when is_integer(Value) ->
  make_query({Key, integer_to_list(Value)});

make_query({Key, Value}) when is_atom(Key) ->
  make_query({atom_to_list(Key), Value});

make_query({Key, Value}) ->
  [url_encode(Key), "=", url_encode(Value)].

url_encode(Value) when is_list(Value) ->
  http_uri:encode(Value);

url_encode(Value) when is_bitstring(Value) ->
  url_encode(binary_to_list(Value));

url_encode(Value) when is_integer(Value) ->
  Value.
