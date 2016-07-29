%%%-------------------------------------------------------------------
%% @doc simplepool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(simplepool_sup).

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
	Pools = {simplepool_pools_sup, {simplepool_pools_sup, start_link, []},
                permanent, 5000, supervisor, [simplepool_pools_sup]},

	Disp = {simplepool_disp, {simplepool_disp, start_link, []},
                permanent, 5000, worker, [simplepool_disp]},
	{ok, {{one_for_all, 0, 1}, [Pools, Disp]}}.

%%====================================================================
%% Internal functions
%%====================================================================
