## Another Erlang OTP application for managing pools of workers
=====

#### Features:
* Multiple pools each with its' own worker module and optional controller module. Controller gets started
after workers. Controller has list of workers in its' args, workers have controller in their args.
* No trips to ets or gen_servers to get the worker - all the workers are compiled into actual RAM-located
beam module as soon as a pool is created
* It is possible to get all the workers in one call
* Every worker in pool gets assigned an unique name (atom), so it is possible to use `gen_server:cast/call(Name, ....)`
As the atoms are created dynamically it wouldn't be a smart idea to create pools with different names
too often. Luckily, pools are usually static
* So all the gen_servers must be started with `gen_server:start_link/4`, with either {local, Name} or {global, Name}.
See an example module in tests
* No any built-in load-balancing features. You get all the workers in a pool and if you want you may balance
them yourself. The only built-in balancing is random. So if you are going to use synchronous `gen_server:call`
ensure that the callbacks don't take much time.



#### Other erlang pools:
https://github.com/devinus/poolboy and https://github.com/inaka/worker_pool


Build
-----

    $ rebar3 compile
    
    
Tests
-----

    $ rebar3 ct    


Examples
-----
```erlang
%%simplepool:start_pool([Pool name], [Pool size], [Worker module], [Arg for the Init of the module]).
ok = simplepool:start_pool(pool3, 5, test_worker, [1,2,3], #{strategy => one_for_one, intensity => 1, period => 5}, undefined).
{5, Workers3, undefined} = simplepool:pool(pool3).
5 = tuple_size(Workers3).
RandWorker1 = simplepool:rand_worker(pool3).
0 = gen_server:call(RandWorker1, {mul, 2}).
simplepool:stop_pool(pool3),
not_found = simplepool:pool(pool3),
%%global workers are also possible
ok = simplepool:start_pool(global, pool4, 5, test_worker, [1,2,3], #{strategy => one_for_all, intensity => 1, period => 5}, test_controller).
RandWorker2 = simplepool:rand_worker(pool4).
0 = gen_server:call(RandWorker2, {mul, 2}).
```

It is also possible to specify pools in `env` of the `app.src` file.

See the tests for more examples. Example apps: https://github.com/brigadier/tara/ https://github.com/brigadier/geodata2/

