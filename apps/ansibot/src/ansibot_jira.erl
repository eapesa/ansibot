-module(ansibot_jira).

-export([
  new_issue/2,
  get_issue_type/0,
  get_project_id/0
]).

new_issue(Data, Opts) ->
  ProjectName = maps:get(<<"project_name">>, Data, <<"project">>),
  TagName     = maps:get(<<"tag_name">>, Data, <<"v1.0">>),
  Summary     = <<"[", ProjectName/binary, "] ", TagName/binary, " Release">>,

  QeAssignee  = maps:get(<<"qe_assignee">>, Data, list_to_binary(
      ansibot_env:get(jira_qe_default, "quality.engineer"))),
  SeAssignee  = maps:get(<<"se_assignee">>, Data, list_to_binary(
      ansibot_env:get(jira_se_default, "service.engineer"))),
  EngName     = generate_name(maps:get(name, Opts, #{})),
  Description = <<"Hi [\~", QeAssignee/binary, "],\n",
      "FYA. The changes can be found in the release notes link. Thanks!\n\n",
      "Hi [\~", SeAssignee/binary, "],\n",
      "Kindly check with ", EngName/binary,
      " for the config changes in case none is indicated.\n",
      "Else, please deploy. Thanks!\n\n",
      "Filed by: ", EngName/binary>>,

  IssueType   = list_to_binary(ansibot_env:get(jira_issue_name, "Standard Change")),
  TargetEnv   = maps:get(<<"env">>, Data, <<"Staging">>),
  ReleaseNotes = maps:get(<<"release_notes">>, Data, <<"http://release.notes.com">>),
  %% TODO: CHECK THE PROPER FORMAT FOR THIS.
  Now = <<"20170515">>,
  Body = jsx:encode(#{ <<"fields">> => 
    #{
      <<"project">>           => #{
        <<"key">>   => ansibot_env:get(jira_project_key, "DEPLOYMENT")
      },
      <<"summary">>           => Summary,
      <<"description">>       => Description,
      <<"issuetype">>         => #{
        <<"name">>  => IssueType
      },
      <<"assignee">>          => #{
        <<"name">>  => QeAssignee
      },
      <<"customfield_10400">> => #{
        <<"value">> => TargetEnv
      },
      <<"customfield_10402">> => ReleaseNotes,
      <<"customfield_10403">> => TagName,
      <<"customfield_19106">> => Now
    }
  }),
  BaseUrl = ansibot_env:get(jira_api, "https://jira.com/rest/api/2/"),
  Url = BaseUrl ++ "issue",
  AuthKey = list_to_binary(ansibot_env:get(jira_auth_key, "Basic AUTHKEY")),
  Headers = #{ <<"authorization">> => AuthKey, <<"content-type">> => <<"application/json">> },
  Response = util_http:post(Url, Headers, #{}, Body),
  io:format("CREATE ISSUE RESPONSE >>> ~p~n", [Response]),
  case Response of
    #{ status := 200 } ->
      io:format("CREATED! 200~n", []),
      ok;
    #{ status := 201 } ->
      io:format("CREATED! 201~n", []),
      ok;
    Error ->
      Error
  end.

generate_name(#{ first := F, last := L }) -> <<F/binary, " ", L/binary>>;
generate_name(_) -> <<"Conan Edogawa">>.

get_issue_type() ->
  % https://HOST/api/2/issuetype
  BaseUrl = ansibot_env:get(jira_api, "https://jira.com/rest/api/2/"),
  Url = BaseUrl ++ "issuetype",
  AuthKey = list_to_binary(ansibot_env:get(jira_auth_key, "Basic AUTHKEY")),
  Headers = #{ <<"authorization">> => AuthKey, <<"content-type">> => <<"application/json">> },
  Response = util_http:get(Url, Headers),
  case Response of
    #{ status := 200, body := Body } ->
      Map = jsx:decode(Body, [return_maps]),
      IssueName = list_to_binary(ansibot_env:get(jira_issue_name, 
          "Standard Change")),
      case util_io:find_map(<<"name">>, IssueName, Map) of
        false -> {issue_type_id, not_found};
        IssueDetails ->
          {issue_type_id, maps:get(<<"id">>, IssueDetails)}
      end;
    Error ->
      Error
  end.

get_project_id() ->
  % https://HOST/api/2/project/SEDSR
  BaseUrl = ansibot_env:get(jira_api, "https://jira.com/rest/api/2/"),
  ProjectName = ansibot_env:get(jira_project_key, "DEPLOYMENT"),
  Url = BaseUrl ++ "project/" ++ ProjectName,
  AuthKey = list_to_binary(ansibot_env:get(jira_auth_key, "Basic AUTHKEY")),
  Headers = #{ <<"authorization">> => AuthKey, <<"content-type">> => <<"application/json">> },
  Response = util_http:get(Url, Headers),
  case Response of
    #{ status := 200, body := Body } ->
      Map = jsx:decode(Body, [return_maps]),
      {project_id, maps:get(<<"id">>, Map, <<"1">>)};
    Error ->
      Error
  end.

% 'fields': {
%   'project': {
%     'key': jira.projectKey
%   },
%   'summary': params.summary,
%   'description': params.description,
%   'issuetype': {
%     'name': 'Deployment'
%   },
%   'assignee': {
%     'name': params.assignee
%   },
%   'customfield_10400': {
%     'value': params.targetEnv
%   },
%   'customfield_10402': params.releaseNotesLink,
%   'customfield_10403': params.versionTag
% }