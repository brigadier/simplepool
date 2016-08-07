-module(gen_simplepool_worker).

%% API
-export([]).

-type pool_neighbours() :: atom() | [atom()].

-callback simplepool_start_link(Visibility :: local|global, Name :: atom(), Neighbours :: pool_neighbours(), Args :: term()) ->
    {ok, pid()} | ignore | {error, term()}.