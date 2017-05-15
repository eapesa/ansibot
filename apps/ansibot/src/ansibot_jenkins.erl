-module(ansibot_jenkins).
-export([
  build/2
]).

build(ProjectName, TagName) ->
  BaseUrl = ansibot_env:get(jenkins_api, "https://jenkins.com/job/"),
  Url = BaseUrl ++ binary_to_list(<<ProjectName/binary, "/buildWithParameters">>),
  Response = util_http:post(Url, #{ <<"content-type">> => <<"application/xml">> },
      #{ <<"GIT_TAG">> => TagName }),
  case Response of
  	#{ status := 200 } -> ok;
    #{ status := 201 } -> ok;
    Error -> Error
  end.