= Lsys =

A set of Erlang applications for experimenting with L-Systems, and using L-Systems
in your own applications.

Once the lsys application is started you can use like so:

 {ok, Config} = lsys:read_config([code:priv_dir(lsys)], "test1.lsys"),
 Axiom = "S0",
 Iters = 3,
 lsys_srv:generate(Iters, Axiom, Config).

Alternatively you can use without a gen_server or supervision tree:

 {ok, Config} = lsys:read_config(["priv"], "test1.lsys"),
 Axiom = "S0",
 Iters = 3,
 lsys:generate(Iters, Axiom, Config).
 
