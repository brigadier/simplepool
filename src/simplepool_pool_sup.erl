-module(simplepool_pool_sup).

-behaviour(supervisor).
-define(SERVER, ?MODULE).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/6]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Visibility, SupName, Workers, WorkerModule, WorkerArgs, SupFlags) ->
	supervisor:start_link(
		{local, SupName},
		?MODULE, {Visibility, Workers, WorkerModule, WorkerArgs, SupFlags}
	).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init({Visibility, WorkersList, WorkerModule, WorkerArgs, SupFlags}) ->
	Workers = get_workers(Visibility, WorkersList, WorkerModule, WorkerArgs),
	{ok, {SupFlags, Workers}}.

%%{one_for_one, 1, 5}

%%%===================================================================
%%% Internal functions
%%%===================================================================


get_workers(Visibility, WorkersList, WorkerModule, WorkerArgs) ->
	lists:map(
		fun(W) ->
			{W, {WorkerModule, simplepool_start_link, [Visibility, W | WorkerArgs]},
				transient, 5000, worker, [WorkerModule]}
		end,
		WorkersList
	).
