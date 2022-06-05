-module(plasticity).
-export([all/0, random_plasticity/1, get_rand_plast/2, get_plasticity/2, apply_plasticity/3]).
-define(PLAST(Weight), [?RAND || _ <- lists:seq(1, length(Weight))]).
-include("utils.hrl").

get_rand_plast(Weight, PlastChoiches) ->
	NewPlast = random_plasticity(PlastChoiches),
	get_plasticity(Weight, NewPlast).

random_plasticity(PlastChoiches) ->
	NewPlast = ?RANDCHOOSE(PlastChoiches),
	case NewPlast of
		neuromod -> {neuromod, ?RANDCHOOSE(lists:seq(0, 5))};
		_ -> NewPlast
	end.

all() -> 
	[none, hebbian, oja, neuromod].

get_plasticity(_, none) -> 
	none;
get_plasticity(Weight, hebbian) -> 
	{hebbian, ?PLAST(Weight)};
get_plasticity(Weight, oja) -> 
	{oja, ?PLAST(Weight)};
get_plasticity(Weight, neuromod) -> 
	get_plasticity(Weight, {neuromod, 0});
get_plasticity(Weight, {neuromod, N}) when N =< 5,N >= 0 ->
	NeuronModulators = [{?PLAST(Weight), ?RAND} || _ <- lists:seq(1, N)],
	NormalModulators = [?PLAST(Weight) || _ <- lists:seq(1, 5 - N)],
	{neuromod, N, NeuronModulators ++ NormalModulators}.

apply_plasticity(Weights, Signals, Output)->	
	apply_plasticity(Weights, Signals, Output,[]).

apply_plasticity([], _, _, Acc) -> 
	Acc;
apply_plasticity([{Id, NodeType, Weight, none} | T], Signals, Output, Acc) ->
	apply_plasticity(T, Signals, Output, Acc ++ [{Id, NodeType, Weight, none}]);
apply_plasticity([{Id, NodeType, Weight, {hebbian, LearnParams}} | T], Signals, Output, Acc) ->
	{Id, Sig} = lists:keyfind(Id, 1, Signals),
	NewInWeight = {Id, NodeType, hebbian(Weight, LearnParams, Sig, Output), {hebbian, LearnParams}},
	apply_plasticity(T, Signals, Output, Acc ++ [NewInWeight]);
apply_plasticity([{Id, NodeType, Weight, {oja, LearnParams}} | T], Signals, Output, Acc) ->
	{Id, Sig} = lists:keyfind(Id, 1, Signals),
	NewInWeight = {Id, NodeType, oja(Weight, LearnParams, Sig, Output), {oja, LearnParams}},
	apply_plasticity(T, Signals, Output, Acc ++ [NewInWeight]);
apply_plasticity([{Id, NodeType, Weight, {neuromod, N, LearnParams}} | T], Signals, Output, Acc) ->
	{Id, Sig} = lists:keyfind(Id, 1, Signals),
	NewInWeight = {Id, NodeType, neuromod(N, Weight, LearnParams, Sig, Output), {neuromod, N, LearnParams}},
	apply_plasticity(T, Signals, Output, Acc ++ [NewInWeight]).

hebbian(Weight, LearnParams, Sig, Output) ->%W(t+1)=W(t)+H*InputSignal*Output
	A = math_utils:lists_elements_dot(Sig, Output),
	B = math_utils:lists_elements_dot(LearnParams, A),
	C = math_utils:lists_elements_sum(Weight,B),
	[math_utils:saturate(El, -?SAT_LIMIT, ?SAT_LIMIT) || El <- C].

oja(Weight, LearnParams, Sig, Output)->%W(t+1)=W(t)+H*Output*(InputSignal–O*W(t))
	A = math_utils:lists_elements_sub(Sig, math_utils:lists_elements_dot(Weight, Output)),
	B = math_utils:lists_elements_dot(Output, A),
	C = math_utils:lists_elements_dot(LearnParams, B),
	D = math_utils:lists_elements_sum(Weight, C),
	[math_utils:saturate(El, -?SAT_LIMIT, ?SAT_LIMIT) || El <- D].

neuromod(N, Weight, LearnParams, Sig, Output)->%W(t+1)=W(t)+H*(A*InputSignal*Output+B*InputSignal+C*Output+D),
	{Modulator, NotModulator} = lists:split(N, LearnParams),
	[H , A, B, C, D] = [ [modulate(Mod, Sig)] || Mod <- Modulator] ++ NotModulator,
	P1 = math_utils:lists_elements_dot(A, math_utils:lists_elements_dot(Sig, Output)),
	P2 = math_utils:lists_elements_dot(B, Sig),
	P3 = math_utils:lists_elements_dot(C, Output),
	P4 = math_utils:lists_elements_sum(P1, math_utils:lists_elements_sum(P2, math_utils:lists_elements_sum(P3, D))),
	P5 = math_utils:lists_elements_dot(H, P4),
	P6 = math_utils:lists_elements_sum(Weight, P5),
	[math_utils:saturate(El, -?SAT_LIMIT, ?SAT_LIMIT) || El <- P6].

modulate({Weight,Bias},Sig)->
	DotSum = lists:sum(math_utils:lists_elements_dot(Weight, Sig)),
	ModulateFun = fun() -> af:tanh(DotSum + Bias) end,
	math_utils:catch_overflow(ModulateFun , ?SAT_LIMIT).