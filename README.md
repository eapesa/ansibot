ansibot
=======

An OTP application

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
|=================|========|=============================|
| *@tag_name*     | string | Tag name of your deployment |
| *@project_name* | string | Project name                |


## /build 

### Example Syntax

`/tag @tag_name TAG_NAME @project_name PROJECT_NAME`

### Parameters

| Parameter       | Type   | Description                 |
|=================|========|=============================|
| *@tag_name*     | string | Tag name of your deployment |
| *@project_name* | string | Project name                |

