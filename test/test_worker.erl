-module(test_worker).

-behaviour(gen_server).
-behaviour(gen_simplepool_worker).
%% API
-export([simplepool_start_link/3]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {mul}).

%%%===================================================================
%%% API
%%%===================================================================

%%start_link(Name, Args) ->
%%	gen_server:start_link({local, Name}, ?MODULE, Args, []).
simplepool_start_link(Visibility, Name, Args) ->
	gen_server:start_link({Visibility, Name}, ?MODULE, Args, []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init(Args) ->
	Mul = proplists:get_value(mul, Args, 0),
	{ok, #state{mul = Mul}}.


handle_call(sleep, _From, State) ->
	timer:sleep(1000),
	{reply, ok, State};

handle_call({mul, Mul1}, _From, #state{mul = Mul} = State) ->
	{reply, Mul1 * Mul, State};

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
