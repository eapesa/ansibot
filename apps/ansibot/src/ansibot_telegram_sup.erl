%%%-------------------------------------------------------------------
%% @doc ansibot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ansibot_telegram_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [

      {
        ansibot_telegram_worker,
        { ansibot_telegram_worker, start_link, [] },
        permanent,
        brutal_kill,
        supervisor,
        [ansibot_telegram_worker]
      }

    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
