-module(test_controller).

-behaviour(gen_server).
-behaviour(gen_simplepool_worker).
-include_lib("eunit/include/eunit.hrl").
%% API
-export([simplepool_start_link/4]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {add, workers}).

%%%===================================================================
%%% API
%%%===================================================================

simplepool_start_link(Visibility, Name, Workers, Args) ->
	gen_server:start_link({Visibility, Name}, ?MODULE, [Workers | Args], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Workers | Args]) ->
	Add = proplists:get_value(add, Args, 0),
	{ok, #state{add = Add, workers = Workers}}.


handle_call({mul, Mul1}, _From, #state{add = Add, workers = Workers} = State) ->
	R = lists:foldl(
		fun(Worker, Acc) ->
			gen_server:call(Worker, {mul, Mul1}) + Acc
		end,
		Add,
		Workers
	),
	{reply, R, State};

handle_call(exc, _From, State) ->
	exit(exc),
	{reply, ok, State};
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
