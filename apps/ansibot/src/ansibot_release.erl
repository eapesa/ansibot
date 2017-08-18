-module(ansibot_release).
-export([
  generate/1
]).

%% TODO: All github HTTP request, migrate to `ansibot_github`

generate(Params) ->
  case sanitize_params(Params) of
    {ok, NewParams} ->
      BaseUrl    = ansibot_env:get(github_api, <<"https://git.corp.voyager.ph/api/v3">>),
      Token      = ansibot_env:get(github_token, <<"_GIT_TOKEN_">>),
      StyleSheet = ansibot_env:get(stylesheet,
        <<"http://voyager-dev.s3.amazonaws.com/runic/release_notes/styles/styles.css">>),
      JiraUrl    = ansibot_env:get(jira_url, <<"https://jira.corp.voyager.ph/browse/">>),
      JiraApi    = ansibot_env:get(jira_api, <<"https://jira.corp.voyager.ph/rest/api/2/issue/">>),
      JiraToken  = ansibot_env:get(jira_token, <<"_JIRA_TOKEN_">>),
      desc_project(maps:merge(NewParams, #{
        <<"git_url">>    => BaseUrl,
        <<"git_token">>  => Token,
        <<"jira_url">>   => JiraUrl,
        <<"jira_api">>   => JiraApi,
        <<"jira_token">> => JiraToken,
        <<"stylesheet">> => StyleSheet
      }));
    Error ->
      Error
  end.

desc_project(#{ <<"git_url">> := BaseUrl, <<"git_token">> := Token,
    <<"project_name">> := Name } = Params) ->
  FinalUrl = <<(util_format:ensure_binary(BaseUrl))/binary, "/projects/search/",
      (util_format:ensure_binary(Name))/binary>>,
  Result   = util_http:get(binary_to_list(FinalUrl), #{},
      #{ <<"private_token">> => Token }),
  case Result of
    #{ status := 200, body := Body } ->
      BodyMap = jsx:decode(Body, [return_maps]),
      desc_project(Params, BodyMap);
    Error ->
      Error
  end.

desc_project(_Params, []) -> {error, not_found};
desc_project(#{ <<"project_name">> := Name, <<"tag_compare">> := false } = Params,
    [#{ <<"name">> := Name, <<"path_with_namespace">> := Path,
    <<"id">> := Id } | _Tail]) ->
  desc_tag(maps:merge(Params, #{
    <<"git_id">>    => Id,
    <<"namespace">> => get_namespace(Path)
  }));
desc_project(#{ <<"project_name">> := Name } = Params,
    [#{ <<"name">> := Name, <<"path_with_namespace">> := Path,
    <<"id">> := Id } | _Tail]) ->
  compare_tags(maps:merge(Params, #{
    <<"git_id">>    => Id,
    <<"namespace">> => get_namespace(Path)
  }));
desc_project(Params, [_Head | Tail]) ->
  desc_project(Params, Tail).

desc_tag(#{ <<"git_url">> := BaseUrl, <<"git_token">> := Token,
    <<"tag_name">> := Tag, <<"git_id">> := GitId } = Params)->
  FinalUrl = <<(util_format:ensure_binary(BaseUrl))/binary, "/projects/",
      (util_format:ensure_binary(GitId))/binary, "/repository/tags">>,
  Result   = util_http:get(binary_to_list(FinalUrl), #{},
      #{ <<"private_token">> => Token }),
  case Result of
    #{ status := 200, body := Body } ->
      TagList = jsx:decode(Body, [return_maps]),
      case get_previous_tag(Tag, TagList) of
        {tag, CompareTag} ->
          compare_tags(maps:put(<<"tag_compare">>, CompareTag, Params));
        Error2 ->
          Error2
      end;
    Error ->
      Error
  end.

compare_tags(#{ <<"tag_name">> := _Tag, <<"tag_compare">> := initial }) ->
  io:format("Initial tag. Get all commits.~n", []);
compare_tags(#{ <<"git_url">> := BaseUrl, <<"git_token">> := Token,
    <<"git_id">> := GitId, <<"tag_name">> := Tag,
    <<"tag_compare">> := CompareTag } = Params) ->
  FinalUrl = <<(util_format:ensure_binary(BaseUrl))/binary, "/projects/",
      (util_format:ensure_binary(GitId))/binary, "/repository/compare">>,
  Result   = util_http:get(binary_to_list(FinalUrl), #{},
      #{<<"private_token">> => Token, <<"from">> => CompareTag,
        <<"to">> => Tag}),
  case Result of
    #{ status := 200, body := Body } ->
      organize(Params, jsx:decode(Body, [return_maps]));
    Error ->
      Error
  end.

organize(#{ <<"project_name">> := Name,
            <<"author">>       := Author,
            <<"tag_name">>     := Tag,
            <<"tag_compare">>  := TagCompare,
            <<"stylesheet">>   := StyleSheet } = Params,
         #{ <<"commits">>      := Commits,
            <<"diffs">>        := Diffs }) ->
  {ok, Template} = file:read_file("../../lib/ansibot/src/template.html"),
  {ok, CommitsTemplate, JiraIssues} = format_commits(Commits, Params),
  {ok, JiraTemplates} = format_jira(JiraIssues, Params),
  {ok, DiffsTemplate} = format_diffs(Diffs, Params),
  NewTemplate  = binary:replace(Template, [<<"\n">>], <<>>, [global]),
  NewTemplate0 = binary:replace(NewTemplate, [<<"{{STYLESHEET}}">>],
      StyleSheet, [global]),
  NewTemplate1 = binary:replace(NewTemplate0, [<<"{{JIRA_ISSUES}}">>],
      JiraTemplates, [global]),
  NewTemplate2 = binary:replace(NewTemplate1, [<<"{{TAG_COMMITS}}">>],
      CommitsTemplate, [global]),
  NewTemplate3 = binary:replace(NewTemplate2, [<<"{{TAG_DIFFS}}">>],
      DiffsTemplate, [global]),
  NewTemplate4 = binary:replace(NewTemplate3, [<<"{{TITLE_1}}">>],
      Name, [global]),
  NewTemplate5 = binary:replace(NewTemplate4, [<<"{{TITLE_2}}">>],
      <<Tag/binary, " Release Notes">>, [global]),
  NewTemplate6 = binary:replace(NewTemplate5, [<<"{{AUTHOR}}">>],
      Author, [global]),
  NewTemplate7 = binary:replace(NewTemplate6, [<<"{{DIFF_TITLE}}">>],
      <<"diff from ", TagCompare/binary, " to ", Tag/binary>>, [global]),
  upload_to_s3(NewTemplate7, Params).

upload_to_s3(Html, #{ <<"project_name">> := Name, <<"tag_name">> := Tag }) ->
  {ok, AwsConfig} = erlcloud_aws:auto_config(),
  ServiceConfig   = erlcloud_aws:service_config(s3, list_to_binary(
    ansibot_env:get(dynamodb_region, "ap-southeast-1")), AwsConfig),
  {ok, NewAwsConfig} = erlcloud_aws:configure(ServiceConfig),
  AwsS3Acl  = ansibot_env:get(aws_s3_acl, public_read),
  AwsS3Url  = ansibot_env:get(aws_s3_url, "http://bucket.s3.amazonaws.com/"),
  AwsBucket = ansibot_env:get(aws_s3_bucket, "bucket"),
  AwsS3Key  = ansibot_env:get(aws_s3_key, "key"),
  AwsS3Key2 = AwsS3Key ++ "/" ++ binary_to_list(Name) ++ "/"
      ++ binary_to_list(Tag) ++ ".html",
  Size = integer_to_binary(size(Html)),
  Response = erlcloud_s3:put_object(AwsBucket, AwsS3Key2, binary_to_list(Html),
    [{acl, AwsS3Acl}], [{"content-type", "text/html"}, {"content-length", Size}],
    NewAwsConfig),
  case Response of
    [{version_id, _Null}] ->
      DownloadUrl = list_to_binary(AwsS3Url ++ AwsS3Key2),
      {ok, DownloadUrl};
    _ ->
      Response
  end.

get_namespace(Path) ->
  lists:nth(1, binary:split(Path, [<<"/">>], [global])).

get_previous_tag(Tag, [#{ <<"name">> := Tag}]) ->
  {tag, initial};
get_previous_tag(Tag, TagList) ->
  get_previous_tag(Tag, TagList, false).

get_previous_tag(_Tag, [], _State) -> {error, not_found};
get_previous_tag(_Tag, [#{ <<"name">> := PairTag } | _List], true) ->
  {tag, PairTag};
get_previous_tag(Tag, [#{ <<"name">> := Tag } | List], false) ->
  get_previous_tag(Tag, List, true);
get_previous_tag(Tag, [_Head | List], State) ->
  get_previous_tag(Tag, List, State).

sanitize_params(#{ <<"project_name">> := _ProjectName, <<"tag_name">> := _TagName,
    <<"author">> := _Author } = Params) ->
  JiraProject = maps:get(<<"jira_project">>, Params, <<"CHATBOTS">>),
  Environment = maps:get(<<"env">>, Params, <<"staging">>),
  TagCompare  = maps:get(<<"tag_compare">>, Params, false),
  {ok, maps:merge(Params, #{
    <<"jira_project">> => JiraProject,
    <<"env">>          => Environment,
    <<"tag_compare">>  => TagCompare
  })};
sanitize_params(Params) ->
  {invalid_params, Params}.

format_commits(Commits, Params) ->
  format_commits(Commits, Params, <<>>, []).

format_commits([], _Params, CommitAcc, JiraAcc) ->
  {ok, CommitAcc, JiraAcc};
format_commits([#{<<"id">>           := Id,
                  <<"title">>        := Title,
                  <<"author_name">>  := Author,
                  <<"created_at">>   := CreatedAt
    } | Tail], #{ <<"namespace">>    := Namespace,
                  <<"project_name">> := Name
    } = Params, CommitAcc, JiraAcc) ->
  CommitBase = ansibot_env:get(git_ref, <<"https://git.corp.voyager.ph/">>),
  CommitLink = <<CommitBase/binary, Namespace/binary, "/", Name/binary,
      "/commit/", Id/binary>>,
  Template   = <<"<div id=\"div-commit-", Id/binary, "\" class=\"div-enums\">",
      "<label id=\"commit-title-", Id/binary, "\" class=\"commit-tile\">",
      "<a href=\"", CommitLink/binary, "\" target=\"_blank\">", Title/binary, "</a>"
      "</label>",
      "<br>",
      "<label class=\"commit-author\">", Author/binary, "</label>",
      "<br>",
      "<label class=\"commit-date\">", CreatedAt/binary, "</label>",
      "</div>">>,
  format_commits(Tail, Params, <<CommitAcc/binary, Template/binary>>,
      lists:merge(get_jira_issues(Title, Params), JiraAcc)).

get_jira_issues(Title, #{ <<"jira_project">> := Jira }) ->
  get_jira_issues(binary:split(Title, [<<" ">>], [global]), Jira, []).

get_jira_issues([], _Project, Acc) -> Acc;
get_jira_issues([Head | Tail], Project, Acc) ->
  case binary:match(Head, Project) of
    nomatch ->
      get_jira_issues(Tail, Project, Acc);
    _Match ->
      get_jira_issues(Tail, Project, [Head | Acc])
  end.

format_jira(Jira, Params) ->
  format_jira(Jira, Params, <<>>).

format_jira([], _Params, Acc) -> {ok, Acc};
format_jira([Head | Tail], #{ <<"jira_url">> := BaseUrl } = Params, Acc) ->
  JiraDetails = desc_jira_issue(Head, Params),
  Template = <<"<div class=\"div-enums\">",
      "<label class=\"jira-id\">&bull; &nbsp;",
      "<a href=\"", BaseUrl/binary, Head/binary, "\">",
      Head/binary, (format_jira_summary(JiraDetails))/binary,
      "</a>",
      "</label>",
      (format_jira_assignee(JiraDetails))/binary,
      "</div>">>,
  format_jira(Tail, Params, <<Acc/binary, Template/binary>>).

desc_jira_issue(Issue, #{ <<"jira_api">> := BaseUrl, <<"jira_token">> := Token }) ->
  FinalUrl = binary_to_list(<<BaseUrl/binary, Issue/binary>>),
  Result = util_http:get(FinalUrl, #{ <<"authorization">> => Token }),
  case Result of
    #{ status := 200, body := Body } ->
      BodyMap = jsx:decode(Body, [return_maps]),
      #{
        <<"summary">> => maps:get(<<"summary">>,
            maps:get(<<"fields">>, BodyMap, #{}), <<>>),
        <<"assignee">> => maps:get(<<"assignee">>,
            maps:get(<<"fields">>, BodyMap, #{}), #{})
      };
    Error ->
      Error
  end.

format_jira_summary(#{ <<"summary">> := Summary }) ->
  <<" - ", Summary/binary>>;
format_jira_summary(_) -> <<>>.

format_jira_assignee(#{ <<"assignee">> := #{ <<"displayName">> := Name }}) ->
  <<"<div class=\"jira-subtask\"> Assignee: ", Name/binary, "</div>">>;
format_jira_assignee(_) ->
  <<>>.

format_diffs(Diffs, Params) ->
  format_diffs(Diffs, Params, <<>>).

format_diffs([], _Params, Acc) -> {ok, Acc};
format_diffs([#{ <<"diff">> := Diff } | Tail], Params, Acc) ->
  Template = <<"<div class=\"div-enums div-diff-class\">",
      (color_diffs(Diff))/binary, "</div>">>,
  format_diffs(Tail, Params, <<Acc/binary, Template/binary>>).

color_diffs(Diff) ->
  color_diffs(binary:split(Diff, [<<"\n">>], [global]), <<>>).

color_diffs([], Acc) -> Acc;
color_diffs([<<"-", _Text/binary>> = Head | Tail], Acc) ->
  Template = <<"<label class=\"removed\">", Head/binary,
      "</label><br>">>,
  color_diffs(Tail, <<Acc/binary, Template/binary>>);
color_diffs([<<"+", _Text/binary>> = Head | Tail], Acc) ->
  Template = <<"<label class=\"added\">", Head/binary,
      "</label><br>">>,
  color_diffs(Tail, <<Acc/binary, Template/binary>>);
color_diffs([Head | Tail], Acc) ->
  Template = <<"<label>", Head/binary, "</label><br>">>,
  color_diffs(Tail, <<Acc/binary, Template/binary>>).

%% ansibot_release:generate(#{ <<"project_name">> => <<"ratatoskr-chatbot">>, <<"tag_name">> => <<"v20170815-01">>, <<"tag_compare">> => <<"v20170811-01">>, <<"author">> => <<"Elixa">> }).
