-module(ansibot_telegram_ops).
-export([
  process/4
]).

process(Conn, <<"start">>, Params, Opts)  -> start(Conn, Params, Opts);
process(Conn, <<"help">>, Params, Opts)   -> help(Conn, Params, Opts);
process(Conn, <<"tag">>, Params, Opts)    -> tag(Conn, Params, Opts);
process(Conn, <<"build">>, Params, Opts)  -> build(Conn, Params, Opts);
process(Conn, <<"file">>, Params, Opts)   -> file(Conn, Params, Opts);
process(Conn, <<"release_notes">>, Params, Opts) -> release_notes(Conn, Params, Opts).

start(Conn, _Params, #{name := #{ first := F }} = Opts) ->
  NewOpts = maps:put(text, <<"Hello *", F/binary, "*! How can I help you with your deployment?">>,
    Opts),
  ansibot_telegram_worker:reply(Conn, send_message, NewOpts).

% help(Conn, <<"release_notes">>, Opts) ->
%   HelpMessage = "For generating release notes: \r\n",
%     "*@release_tag* `TAG_NAME` (required) :: Tag of the release. Default value is date today.\r\n",
%     "*@compare_tag* `TAG_NAME` (optional) :: Where the release will be compared. Default value should be the",
%         "last tag before *release_tag*",
%     "*@author* `AUTHOR` (optional) :: Author of the release. Default value would be your name here in telegram.",
%     "*@jira_project* `JIRA_PROJECT` (required) :: Jira project to check in commit messages.\r\n",
%   NewOpts = maps:put(text, list_to_binary(HelpMessage), Opts),
%   ansibot_telegram_worker:reply(Conn, send_message, NewOpts);
help(Conn, _Params, Opts) ->
  HelpMessage = <<"For details on the commands, please refer to this link: ",
    "https://git.corp.voyager.ph/eapesa/ansibot">>,
  NewOpts = maps:put(text, HelpMessage, Opts),
  ansibot_telegram_worker:reply(Conn, send_message, NewOpts).

release_notes(Conn, Params, #{ name := #{ first := F, last := L }} = Opts) ->
  Name = <<F/binary, " ", L/binary>>,
  Result = ansibot_release:generate(maps:put(<<"author">>, Name, Params)),
  case Result of
    {ok, DownloadUrl} ->
      Text = <<"Successfully created release notes! You may access it here: ",
          DownloadUrl/binary>>,
      NewOpts = maps:put(text, Text, Opts),
      ansibot_telegram_worker:reply(Conn, send_message, NewOpts);
    Error ->
      io:format("[tag] Encountered error: ~p~n", [Error]),
      NewOpts = maps:put(text, <<"Encountered some errors while generating release ",
          " notes. Please try again later.">>, Opts),
      ansibot_telegram_worker:reply(Conn, send_message, NewOpts)
  end.

tag(Conn, #{ <<"project_name">> := ProjectName, <<"tag_name">> := TagName }, Opts) ->
  case ansibot_github:new_tag(ProjectName, TagName) of
    ok ->
      NewOpts = maps:put(text, <<"Successfully created tag!">>, Opts),
      ansibot_telegram_worker:reply(Conn, send_message, NewOpts);
    Error ->
      io:format("[tag] Encountered error: ~p~n", [Error]),
      NewOpts = maps:put(text, <<"Encountered some errors while generating new tag.",
          " Please try again later.">>, Opts),
      ansibot_telegram_worker:reply(Conn, send_message, NewOpts)
  end;
tag(Conn, _Data, Opts) ->
  NewOpts = maps:put(text, <<"Invalid syntax.">>, Opts),
  ansibot_telegram_worker:reply(Conn, send_message, NewOpts).

build(Conn, #{ <<"project_name">> := ProjectName, <<"tag_name">> := TagName }, Opts) ->
  case ansibot_jenkins:build(ProjectName, TagName) of
    ok ->
      NewOpts = maps:put(text, <<"Successfully queued build in Jenkins!">>, Opts),
      ansibot_telegram_worker:reply(Conn, send_message, NewOpts);
    Error ->
      io:format("[build] Encountered error: ~p~n", [Error]),
      NewOpts = maps:put(text, <<"Encountered some errors while generating new tag.",
          " Please try again later.">>, Opts),
      ansibot_telegram_worker:reply(Conn, send_message, NewOpts)
  end;
build(Conn, _Data, Opts) ->
  NewOpts = maps:put(text, <<"Invalid syntax.">>, Opts),
  ansibot_telegram_worker:reply(Conn, send_message, NewOpts).

file(Conn, #{ <<"project_name">> := _ProjectName, <<"tag_name">> := _TagName,
    <<"env">> := _Env, <<"release_notes">> := _Notes } = Data, Opts) ->
  case ansibot_jira:new_issue(Data, Opts) of
    ok ->
      NewOpts = maps:put(text, <<"Successfully created issue in Jira!">>, Opts),
      ansibot_telegram_worker:reply(Conn, send_message, NewOpts);
    Error ->
      io:format("[build] Encountered error: ~p~n", [Error]),
      NewOpts = maps:put(text, <<"Encountered some errors while creating issue.",
          " Please try again later.">>, Opts),
      ansibot_telegram_worker:reply(Conn, send_message, NewOpts)
  end;
file(Conn, _Data, Opts) ->
  NewOpts = maps:put(text, <<"Invalid syntax.">>, Opts),
  ansibot_telegram_worker:reply(Conn, send_message, NewOpts).
