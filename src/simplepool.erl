-module(simplepool).
-include("simplepool.hrl").
%% API
-export([start/0]).

-export([start_pool/4, stop_pool/1, pool/1, rand_worker/1, start_pool/5]).
%%%===================================================================
%%% API
%%%===================================================================

start() ->
	true = ensure_started(simplepool).





-spec start_pool(atom(), pos_integer(), atom(), term()) -> {error, term()} | ok.
start_pool(Name, Size, Worker, Args) ->
	start_pool(local, Name, Size, Worker, Args).

-spec start_pool(global|local, atom(), pos_integer(), atom(), term()) -> {error, term()} | ok.
start_pool(Visibility, Name, Size, Worker, Args) ->
	simplepool_disp:start_pool(Visibility, Name, Size, Worker, Args).

-spec stop_pool(atom()) -> {error, term()} | ok.
stop_pool(Name) ->
	simplepool_disp:stop_pool(Name).


-spec pool(atom()) -> not_found | {N :: pos_integer(), Workers :: tuple()}.
pool(Name) ->
	?POOLS_MODULE:pool(Name).


%%you can make your own, better algo of selecting workers, instead of this
-spec rand_worker(atom()) -> not_found | atom() | {global, atom()}.
rand_worker(Name) ->
	case pool(Name) of
		not_found -> not_found;
		{N, Workers} ->
			%%Looks like unique_integer is always a multiple of number of CPUs. So for 4 CPUs and 4 workers it will always return 2
%%			I = (erlang:unique_integer([positive]) rem N) + 1,
			I = erlang:phash2(erlang:unique_integer([positive]), N) + 1,
			element(I, Workers)
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ensure_deps_started(App) ->
	Deps = case application:get_key(App, applications) of
			   undefined -> [];
			   {_, V} -> V
		   end,
	lists:all(fun ensure_started/1,Deps).

ensure_started(App) ->
	application:load(App),
	ensure_deps_started(App)
		andalso case application:start(App) of
					ok ->
						true;
					{error, {already_started, App}} ->
						true;
					Else ->
						error_logger:error_msg("Couldn't start ~p: ~p", [App, Else]),
						false
				end.