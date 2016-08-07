-module(simplepool_pool_sup).

-behaviour(supervisor).
-define(SERVER, ?MODULE).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/8]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Visibility, SupName, Workers, WorkerModule, ControllerName, Controller, WorkerArgs, SupFlags) ->
	supervisor:start_link(
		{local, SupName},
		?MODULE, {Visibility, Workers, WorkerModule, ControllerName, Controller, WorkerArgs, SupFlags}
	).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init({Visibility, WorkersList, WorkerModule, ControllerName, Controller, WorkerArgs, SupFlags}) ->
	Workers = get_workers(Visibility, WorkersList, WorkerModule, WorkerArgs, ControllerName),
	ControllerSpec = get_controller(Visibility, ControllerName, Controller, WorkerArgs, WorkersList),
	{ok, {SupFlags, Workers ++ ControllerSpec}}.

%%{one_for_one, 1, 5}

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_controller(_Visibility, _ControllerName, undefined, _WorkerArgs, _WorkersList) -> [];
get_controller(Visibility, ControllerName, Controller, WorkerArgs, WorkersList) ->
	[{ControllerName, {Controller, simplepool_start_link, [Visibility, ControllerName, WorkersList | WorkerArgs]},
		transient, 5000, worker, [Controller]}].

get_workers(Visibility, WorkersList, WorkerModule, WorkerArgs, ControllerName) ->
	lists:map(
		fun(W) ->
			{W, {WorkerModule, simplepool_start_link, [Visibility, W, ControllerName | WorkerArgs]},
				transient, 5000, worker, [WorkerModule]}
		end,
		WorkersList
	).
