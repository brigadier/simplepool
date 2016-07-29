-module(simplepool_disp).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include("simplepool.hrl").
%% API
-export([start_link/0, start_pool/4, stop_pool/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pools = []}).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_pool(Name, Size, Worker, Args) when is_atom(Name), is_integer(Size), is_atom(Worker) ->
	gen_server:call(?SERVER, {start, Name, Size, Worker, Args}).

stop_pool(Name) ->
	gen_server:call(?SERVER, {stop, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
	PoolsEnv = application:get_env(simplepool, pools, []),
	Pools = lists:map(
		fun({PoolName, PoolOptions, Args}) ->
			Size = proplists:get_value(size, PoolOptions, 10),
			Worker = proplists:get_value(worker, PoolOptions),
			{ok, Pool} = do_start_pool(PoolName, Size, Worker, [Args]),
			Pool
		end,
		PoolsEnv
	),
	{module, _} = simplepool_builder:build(Pools),
	{ok, #state{pools = Pools}}.


handle_call({start, Name, Size, Worker, Args}, _From, #state{pools = Pools} = State) ->
	case lists:keyfind(Name, 2, Pools) of
		false ->
			case do_start_pool(Name, Size, Worker, [Args]) of
				{ok, Pool} ->
					Pools2 = [Pool | Pools],
					{module, _} = simplepool_builder:build(Pools2),
					{reply, ok, State#state{pools = Pools2}};
				{error, Error} ->
					{reply, {error, Error}, State}
			end;
		_ ->
			{reply, {error, exists}, State}
	end;


handle_call({stop, Name}, _From, #state{pools = Pools} = State) ->
	case lists:keytake(Name, 2, Pools) of
		false ->
			{reply, {error, not_found}, State};
		{value, Pool, Pools2} ->
			Result = do_stop_pool(Pool),
			{module, _} = simplepool_builder:build(Pools2),
			{reply, Result, State#state{pools = Pools2}}

	end;



handle_call(_Request, _From, State) ->
	{reply, ok, State}.



handle_cast(_Request, State) ->
	{noreply, State}.



handle_info(_Info, State) ->
	{noreply, State}.



terminate(_Reason, _State) ->
	ok.



code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_stop_pool(#pool{sup_name = SupName}) ->
	Result = supervisor:terminate_child(simplepool_pools_sup, SupName),
	supervisor:delete_child(simplepool_pools_sup, SupName),
	Result.

do_start_pool(Name, Size, Worker, Args) ->
	WorkerNames = worker_names(Name, Size),
	SupName = sup_name(Name),
	case supervisor:start_child(
		simplepool_pools_sup,

		#{id => SupName,
			start => {simplepool_pool_sup, start_link, [SupName, WorkerNames, Worker, Args]},
			restart => transient,
			shutdown => 3000,
			type => supervisor,
			modules => [simplepool_pool_sup]
		}

	) of
		{error, Error} -> {error, Error};
		{ok, _SupPid} -> {ok, #pool{name = Name, sup_name = SupName, workers = WorkerNames}}
	end.



worker_names(Name, Size) ->
	lists:map(
		fun(I) ->
			list_to_atom(lists:flatten(io_lib:format("$simplepoolworker$~s-~B", [Name, I])))
		end,
		lists:seq(0, Size - 1)
	).


sup_name(Name) -> list_to_atom(lists:flatten(io_lib:format("$simplepoolsup$~s", [Name]))).