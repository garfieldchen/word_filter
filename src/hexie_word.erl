-module(hexie_word).
% -exprot([new/0, new/1, insert/2, filter/2, check/2]).
-compile(export_all).
-define(MASK, $*).

-record(node, {
	key,
	children = [],
	leaf = false
}).

-record(hxdict, {
	mask,
	book = dict:new(),
	tree = #node{key = not_a_key}
}).

-record(state, {
	topnode,
	mask,

	sentence = "",
	wording = "",

	tail,

	sentence_break = "",
	wording_break = ""
}).

new(Mask) ->
	#hxdict{mask = Mask}.

new() ->
	new(?MASK).

%%
insert([_ | _] = Sentence, #hxdict{book = Book, tree = Tree} = Dict) ->
	Book1 = dict:store(Sentence, true, Book),
	Dict#hxdict{book = Book1, tree = insert1(Sentence, Tree)}.

insert1([], Node) ->
	Node#node{leaf = true};

insert1([W | T] = Sentence, #node{children = Chs} = Node) ->
	case lists:keytake(W, #node.key, Chs) of
		false -> 
			Node#node{children = [insert_list(Sentence) | Chs]};
		{value, NodeOld, Rest} ->
			Node#node{children = [insert1(T,  NodeOld)| Rest]}
	end.

insert_list([]) ->
	[];

insert_list(L) ->
	[H | T] = lists:reverse(L),
	lists:foldl(fun(W, Acc) ->
			#node{key = W, children = [Acc]}
		end, #node{key = H, leaf = true}, T).

%%
filter([_ | T] = Sentence, #hxdict{book = Book, tree = Tree, mask = Mask}) ->
	case dict:find(Sentence, Book) of
		error ->
			lists:reverse(filter1(Sentence, Tree, #state{topnode = Tree, tail = T, mask = Mask}));
		_ -> 
			make_mask(?MASK, Sentence)
	end.

filter1([], #node{children = Cs, leaf = Leaf}, 
		#state{wording = Word, sentence = Sen}) when Cs =:= []; Leaf ->
	lists:reverse(lists:append(make_mask(?MASK, Word), Sen));

filter1([], _, #state{wording = Word, sentence = Sen}) ->
	lists:reverse(Word, Sen);

filter1([W | T] = _Sent, #node{children = Cs} = _Node, 
		#state{topnode = TopNode, wording = Word, sentence = Sent} = State) ->
	case lists:keyfind(W, #node.key, Cs) of
		false -> % match failed
			State1 = filter_callback(W, T, State),
			filter1(State1#state.tail, TopNode, State1);
		#node{children = []} -> %match the longest
			Sent2 = lists:append(make_mask(?MASK, [W | Word]), Sent),
			%start new match
			filter1(T, TopNode, State#state{sentence = Sent2, tail = T, wording = "", wording_break = "", sentence_break = ""});
		#node{leaf = true} = Node ->
			State1 = mark_stop(W, T, State),
			filter1(T, Node, State1);
		Node ->
			filter1(T, Node, append_word(W, T, State))
	end.

%%
filter_callback(W, T, #state{wording = "", wording_break = "", sentence = Sent} = State) ->
	State#state{sentence = [W | Sent], wording = "", tail = T};

filter_callback(_W, _T, #state{wording_break = "", wording = Word, sentence = Sent} = State) ->
	State#state{sentence = [lists:last(Word) | Sent], wording = ""};

filter_callback(_, _, #state{sentence_break = Sent, wording_break = Word} = State) ->
	State#state{sentence = lists:append(make_mask(?MASK, Word), Sent), 
				wording = "", sentence_break = "", wording_break = ""}.

%% util
make_mask(Mask, Sentence) ->
	lists:map(fun(_) -> Mask end, Sentence).

append_word(W, T, #state{wording = ""} = State) ->
	State#state{wording = [W], tail = T};

append_word(W, _, #state{wording = Word} = State) ->
	State#state{wording = [W | Word]}.

mark_stop(W, T, #state{wording = Word, sentence = Sent} = State) ->
	Word2 = [W | Word],
	State#state{ wording_break = Word2,
				% sentence_break = [W | Sent],
				sentence_break = Sent,
				wording = Word2,
				% sentence = [W | Sent],
				tail = T}. 

%%test
test() ->
	Ls = ["abc", "123", "12", "abcdef"],
	D = lists:foldl(fun(Str, D1) ->
			insert(Str, D1)
		end, new(), Ls),

	io:format("~p ~n", [D]),
	% io:format("------------------  ~n"),
	% S1 = filter("abcdef", D),
	% io:format("~p ~n", [S1]),

	% S2 = filter("abc", D),
	% io:format("~p ~n", [S2]),

	S3 = filter("abcd123wwwxxxabdefeee", D),
	io:format("~p ~n", [S3]),

	S4 = filter("abcd123wwwxxxabcdefeee", D),
	io:format("~p ~n", [S4]),
	ok.
