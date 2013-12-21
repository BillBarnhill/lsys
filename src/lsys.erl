-module(lsys).
-export([generate/3, read_config/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type config() :: dict().
-type rules() :: dict().
-type constant() :: char() | [].
-type variable() :: char().

-spec read_config(PathList, ConfigFile) -> {ok, Config} | {error, Reason} when
      PathList :: [Dir],
      Dir :: file:name_all(),
      ConfigFile :: file:name_all(),
      Config :: config(),
      Reason :: file:posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.

read_config(PathList, ConfigFile) ->
    case file:path_consult(PathList, ConfigFile) of
	{ok, ConfigTerms, _FullName} -> {ok, dict:from_list(ConfigTerms)};
	Error  -> Error
    end.

-spec generate(Iters, Input, Config) -> string() when
      Iters :: pos_integer(),
      Input :: string(),
      Config :: config().

generate(Iters, Input, Config) ->
    Rules = dict:from_list(dict:fetch(rules, Config)),
    Constants = dict:fetch(constants, Config),
    Vars = dict:fetch(variables, Config),
    lists:flatten(lsys0(Iters, Iters, Input, Rules, Constants, Vars)).


-spec lsys0(ItersLeft, Iters, Input, Rules, Constants, Vars) -> string() when
      ItersLeft :: non_neg_integer(),
      Iters :: pos_integer(),
      Input :: string(),
      Rules :: rules(),
      Constants :: [constant()],
      Vars :: [variable()].
      
lsys0(0, _Iters, Input, _Rules, _Constants, _Vars) ->
    Input;
lsys0(ItersLeft, Iters, Input, Rules, Constants, Vars) ->
    Expansion = lists:flatten(expand(Input, Rules, Constants, Vars)),
    error_logger:info_report([
			      lsys,
			      {iteration, Iters-ItersLeft},
			      {input, Input},
			      {output, Expansion}
			     ]),
    lsys0(ItersLeft-1, Iters, Expansion, Rules, Constants, Vars).

-spec expand(Input, Rules, Constants, Vars) -> iolist() when
      Input :: string(),
      Rules :: rules(),
      Constants :: [constant()],
      Vars :: [variable()].

expand([], _Rules, _Constants, _Vars) ->
    [];
expand(Input, Rules, Constants, Vars) ->
    lists:map(fun 
		  (Part) when is_list(Part) ->
		      expand(Part, Rules, Constants, Vars);
		  (Sym)  ->
		      if_in(Sym, Constants, 
			    fun () -> Sym end, 
			    fun () -> dict:fetch(Sym, Rules) end)
	      end, Input).

-spec if_in(Member, List, ExprTrue, ExprFalse) -> any() when
      Member :: any(),
      List :: [any()],
      ExprTrue :: fun(() -> any()),
      ExprFalse :: fun(() -> any()).
			     
if_in(Member, List, ExprTrue, ExprFalse) ->
    case lists:member(Member, List) of
	true -> ExprTrue();
	false -> ExprFalse()
    end.

-ifdef(TEST).
	
base_test() ->
    io:format("~p~n", [file:get_cwd()]),
    {ok, Config} = read_config(["../priv"], "test1.lsys"),
    Axiom = "S0",
    Iters = 3,
    ?assertEqual("1111[11[1[0]0]1[0]0]11[1[0]0]1[0]0", generate(Iters, Axiom, Config)).

srv_test() ->
    application:start(lsys),
    {ok, Config} = read_config([code:priv_dir(lsys)], "test1.lsys"),
    Axiom = "S0",
    Iters = 3,
    ?assertEqual({ok,"1111[11[1[0]0]1[0]0]11[1[0]0]1[0]0"}, lsys_srv:generate(Iters, Axiom, Config)).
    
-endif.
