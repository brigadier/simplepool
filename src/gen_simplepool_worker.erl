-module(gen_simplepool_worker).

%% API
-export([]).

-callback simplepool_start_link(Visibility :: local|global, Name :: atom(), Args :: term()) ->
	{ok, pid()} | ignore | {error, term()}.