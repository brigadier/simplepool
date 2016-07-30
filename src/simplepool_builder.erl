-module(simplepool_builder).
-include("simplepool.hrl").
-include_lib("eunit/include/eunit.hrl").


%% API
-export([build/1]).


build(Pools) ->
	Module = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(?POOLS_MODULE)]),
	ModForm = erl_syntax:revert(Module),
	Export = erl_syntax:attribute(
		erl_syntax:atom(export), [
			erl_syntax:list([
				erl_syntax:arity_qualifier(erl_syntax:atom(pool), erl_syntax:integer(1))
			])
		]
	),
	ExportForm = erl_syntax:revert(Export),
	Clauses = lists:foldl(
		fun(#pool{name = Name, workers = Workers, visibility = Visibility}, Acc) ->
			WorkersTuple = list_to_tuple(adjust_global(Workers, Visibility)),
			WorkersLen = length(Workers),
			WorkersTree = erl_syntax:abstract({WorkersLen, WorkersTuple}),
			Selector = erl_syntax:atom(Name),

			Clause = erl_syntax:clause([Selector], [], [WorkersTree]),
			[Clause | Acc]

		end,
		[],
		Pools
	),
	Clauses2 = lists:reverse([
		erl_syntax:clause([erl_syntax:variable("_")], [], [erl_syntax:abstract(not_found)]) | Clauses
	]),


	Function = erl_syntax:function(erl_syntax:atom(pool), Clauses2),
	FunctionForm = erl_syntax:revert(Function),

	{ok, Mod, Bin1} = compile:forms([ModForm, ExportForm, FunctionForm]),
	code:load_binary(Mod, [], Bin1).

adjust_global(Workers, local) -> Workers;
adjust_global(Workers, global) -> [{global, W} || W <- Workers].