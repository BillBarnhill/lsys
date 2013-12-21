%%% @author Bill Barnhill <w.a.barnhill@gmail.com>
%%% @copyright 2013 Bill Barnhill.
%%% @doc 

-module(lsys_srv).
-behaviour(gen_server).

-author('Bill Barnhill <w.a.barnhill@gmail.com>').

-export([start_link/0, generate/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/1]).

-record(state, {}).

% public api

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

generate(Iters, Axiom, Config) ->
    gen_server:call(?MODULE, {lsys_generate, Iters, Axiom, Config}).

% state should be change with State that you will pass
init([]) ->
  {ok, #state{} }.

stop(_Pid) ->
  stop().

stop() ->
  gen_server:cast(?MODULE, stop).


%% genserver handles

handle_call({lsys_generate, Iters, Axiom, Config}, _From, State) ->
  Response = {ok, lsys:generate(Iters, Axiom, Config)},
  {reply, Response, State};

handle_call(_Message, _From, State) ->
  {reply, {error, invalid_message}, State}.

handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


% tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-ifdef(TEST).

-endif.
