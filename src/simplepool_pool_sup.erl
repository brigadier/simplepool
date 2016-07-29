-module(simplepool_pool_sup).

-behaviour(supervisor).
-define(SERVER, ?MODULE).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(SupName, Workers, WorkerModule, WorkerArgs) ->
	supervisor:start_link(
		{local, SupName},
		?MODULE, {Workers, WorkerModule, WorkerArgs}
	).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init({WorkersList, WorkerModule, WorkerArgs}) ->
	Workers = get_workers(WorkersList, WorkerModule, WorkerArgs),
	{ok, {{one_for_one, 0, 1}, Workers}}.



%%%===================================================================
%%% Internal functions
%%%===================================================================


get_workers(WorkersList, WorkerModule, WorkerArgs) ->
	lists:map(
		fun(W) ->
			{W, {WorkerModule, start_link, [W|WorkerArgs]},
                transient, 5000, worker, [WorkerModule]}
		end,
		WorkersList
	).
