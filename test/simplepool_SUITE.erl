-module(simplepool_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%% API
-compile(export_all).

all() -> [
	simplepooltest,
	simplepoolcontrollertest].

simplepoolcontrollertest(_Config) ->
	{3, _Workers1, Controller1} = simplepool:pool(poolc1),
	true = Controller1 =/= undefined,
	RandWorker1 = simplepool:rand_worker(poolc1),
	33 = gen_server:call(RandWorker1, {mul, 3}),
	33 * 3 + 2 = gen_server:call(Controller1, {mul, 3}),
	ok = simplepool:stop_pool(poolc1),


	ok = simplepool:start_pool(poolc2, 3, test_worker, [{mul, 11}, {add, 2}], {one_for_all, 1, 1}, test_controller),
	RandWorker2 = simplepool:rand_worker(poolc2),
	33 = gen_server:call(RandWorker2, {mul, 3}),
	Controller2 = simplepool:controller(poolc2),
	33 * 3 + 2 = gen_server:call(Controller2, {mul, 3}),

	case catch gen_server:call(Controller2, exc) of
		{'EXIT', _} -> ok
	end,
	timer:sleep(100), %% let restart once
	Controller2 = simplepool:controller(poolc2),
	case catch gen_server:call(Controller2, exc) of
		{'EXIT', {exc, _}} -> ok
	end,
	timer:sleep(500), %let rebuild beam
	not_found = simplepool:controller(poolc2),
	ok.

simplepooltest(_Config) ->

	not_found = simplepool:pool(pool_no_exists),

	%pools from env
	{2, Workers1, undefined} = simplepool:pool(pool1),
	2 = tuple_size(Workers1),
	{3, Workers2, undefined} = simplepool:pool(pool2),
	3 = tuple_size(Workers2),

	%stop pool from env
	simplepool:stop_pool(pool1),
	not_found = simplepool:pool(pool1),
	{3, Workers2, undefined} = simplepool:pool(pool2),

	%start pool
	ok = simplepool:start_pool(pool3, 5, test_worker, [1,2,3], {one_for_one, 1, 5}, undefined),
	{5, Workers3, undefined} = simplepool:pool(pool3),
	5 = tuple_size(Workers3),
	RandWorker1 = simplepool:rand_worker(pool3),
	0 = gen_server:call(RandWorker1, {mul, 2}),


	%global pool
	ok = simplepool:start_pool(global, pool4, 4, test_worker, [{mul, 3}], #{strategy => one_for_all, intensity => 1, period => 5}, undefined),
	{4, Workers4, undefined} = simplepool:pool(pool4),
	4 = tuple_size(Workers4),
	RandWorker4 = simplepool:rand_worker(pool4),
	6 = gen_server:call(RandWorker4, {mul, 2}),


	%stop just started pool
	simplepool:stop_pool(pool3),
	not_found = simplepool:pool(pool3),

	%get random worker from pool
	RandWorker2 = simplepool:rand_worker(pool2),
	20 = gen_server:call(RandWorker2, {mul, 2}),

	%check that all workers are returned, no one is missed
	RandWorkers = lists:usort([simplepool:rand_worker(pool2) || _ <- lists:seq(1, 200)]),
	3 = length(RandWorkers),


	%check that workers are actually concurrent
	Parent = self(),
	?debugMsg("Spawn 3 parallel threads..."),
	Now = erlang:system_time(seconds),
	lists:foreach(
		fun(W) ->
			spawn(
				fun() ->
					ok = gen_server:call(W, sleep),
					Parent ! ok
				end
			)
		end,
		tuple_to_list(Workers2)
	),
	lists:foreach(
		fun(_) ->
			receive
				ok -> ok
			end
		end,
		tuple_to_list(Workers2)
	),
	true = erlang:system_time(seconds) - Now < 2,

	%that's all
	ok.

init_per_suite(Config) ->
	application:load(simplepool),
	application:set_env(simplepool, pools,
		[
			{pool1, [{size, 2}, {worker, test_worker}, {sup_flags, #{strategy => one_for_one, intensity => 1, period => 5}}], [a,b,c]},
			{pool2, [{size, 3}, {worker, test_worker}], [{mul, 10}]},
			{poolc1, [{size, 3}, {worker, test_worker}, {pool_controller, test_controller}], [{mul, 11}, {add, 2}]}
		]
	),
	simplepool:start(),
	Config.

end_per_suite(_Config) ->
	ok.


init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, _Config) ->
	ok.

init_per_testcase(_TestCase, Config) ->
	Config.

end_per_testcase(_TestCase, _Config) ->
	ok.
