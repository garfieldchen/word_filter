-module(hexie_server).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([filter/1, check/1]).

start_link(Fn) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Fn, []).

init(Fn) ->
	{ok, load_dict(Fn)}.

load_dict(_Fn) ->
	hexie_word:new($*).

filter(Sentence) ->
	gen_server:call({filter, Sentence}).

check(W) ->
	gen_server:call(?MODULE, {check, W}).

handle_call({filter, Sentence}, _From, State) ->
	{reply, hexie_word:filter(Sentence, State), State};

handle_call({check, W}, _From, State) ->
	{reply, hexie_word:check(W, State), State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldSvn, State, _Extra) ->
	{ok, State}.
