-module(hexie_server).

-export([start_link/1]).
-export([filter/1,
		 get_dict/0,
		 check/1]).

start_link(Fn) ->
	D = hexie_word:from_file(Fn),
	mochiglobal:put(?MODULE, D).

get_dict() ->
	mochiglobal:get(?MODULE).

filter(Sen) ->
	D = mochiglobal:get(?MODULE),
	hexie_word:filter(Sen, D).

% it's a bad way
check(Sen) ->
	Sen =:= filter(Sen).
	