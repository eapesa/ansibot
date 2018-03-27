ansibot
=======

An OTP application for automating creation of tags, builds and release notes.

This application intends to hasten the deployment process of the team by automating creation of release notes, git tags and jenkins build.

*NOTE: There might be changes to implement to adjust third party services like gitlab.*

Build
-----

    $ ./rebar3 compile
    $ ./rebar3 release -n ansibot


Deploy
------

    $ _build/default/rel/ansibot/bin/ansibot start | console | stop
    
    
Available Commands
------------------

## /tag 

### Example Syntax

`/tag @tag_name TAG_NAME @project_name PROJECT_NAME`

### Parameters

| Parameter       | Type   | Description                 |
|-----------------|--------|-----------------------------|
| *@tag_name*     | string | Tag name of your deployment |
| *@project_name* | string | Project name                |


## /build 

### Example Syntax

`/tag @tag_name TAG_NAME @project_name PROJECT_NAME`

### Parameters

| Parameter       | Type   | Description                 |
|-----------------|--------|-----------------------------|
| *@tag_name*     | string | Tag name of your deployment |
| *@project_name* | string | Project name                |


## /release_notes 

### Example Syntax

`/release_notes @tag_name TAG_NAME @project_name PROJECT_NAME`

### Parameters

| Parameter       | Required | Description                 |
|-----------------|----------|-----------------------------|
| *@tag_name*     | true     | Tag name of your deployment |
| *@project_name* | true     | Project name                |
| *@env*          | false    | staging or prod. Defaults to *staging*. |
| *@jira_project* | false    | Jira project namespace. Defaults to *CHATBOTS*. |
| *@tag_compare*  | false    | If provided, the diff result of the release notes would be the diff between *tag_name* and *tag_compare*. Defaults to tag before *tag_name* |

