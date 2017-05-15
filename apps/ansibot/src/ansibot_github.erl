-module(ansibot_github).
-export([
  new_tag/2,
  get_project_id/1
]).

new_tag(ProjectName, TagName) ->
  case get_project_id(ProjectName) of
    {ok, Id} ->
      GitUrl    = ansibot_env:get(github_api, "https://git.com/api/v3/"),
      GitToken  = list_to_binary(ansibot_env:get(github_token, "abcde12345")),
      GitRef    = list_to_binary(ansibot_env:get(git_repo_ref, "master")),
      BinId     = integer_to_binary(Id),
      Url       = GitUrl ++ binary_to_list(<<"projects/", BinId/binary, "/repository/tags">>),
      Qs        = #{
        <<"private_token">> => GitToken,
        <<"tag_name">>      => TagName,
        <<"ref">>           => GitRef
      },
      Response = util_http:post(Url, #{}, Qs),
      case Response of
        #{ status := 200 } -> ok;
        #{ status := 201 } -> ok;
        Unknown -> Unknown
      end;
    Error ->
      Error
  end.

get_project_id(ProjectName) ->
  GitUrl    = ansibot_env:get(github_api, "https://git.com/api/v3/"),
  GitToken  = list_to_binary(ansibot_env:get(github_token, "abcde12345")),
  Url       = GitUrl ++ binary_to_list(<<"projects/search/", ProjectName/binary>>),
  Qs        = #{
    <<"private_token">> => GitToken
  },
  Response = util_http:get(Url, #{}, Qs),
  case Response of
    #{ status := 200, body := Body } ->
      Map = jsx:decode(Body, [return_maps]),
      Self = util_io:find_map(<<"path">>, ProjectName, Map),
      {ok, maps:get(<<"id">>, Self, 1)};
    Error ->
      io:format("[get_project_id] Encountered error: ~p~n", [Error]),
      Error
  end.