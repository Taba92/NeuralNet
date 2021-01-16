-module(scape).
-export([init/0,init/1,handle_call/3]).
-record(state,{curRd,truthTable,errAcc}).
-define(POSSIBLEOUTPUTS,2).
-include("utils.hrl").

init()->
	gen_server:start(?MODULE,[],[]).
init([])->
	Xor=[{[0,0],[0]},
		{[1,0],[1]},
		{[0,1],[1]},
		{[1,1],[0]}],
	State=#state{curRd=Xor,truthTable=Xor,errAcc=0},
	{ok,State}.

handle_call(sense,_,State)->
	#state{curRd=[{Signal,_}|_]}=State,
	{reply,Signal,State};
handle_call({action_fit,Predict},_,State)->
	#state{curRd=[{_,Truth}|T],truthTable=Xor,errAcc=ErrAcc}=State,
	Error = list_compare(Predict,Truth,0),
	case T of
		[] ->
			MSE = ErrAcc+Error,
			Fitness = 1-MSE/(length(Xor)*?POSSIBLEOUTPUTS),
			{reply,{Fitness,1},State#state{curRd=Xor,errAcc=0}};
		_ ->
			{reply,{0,0},State#state{curRd=T,errAcc=ErrAcc+Error}}
	end.

list_compare([X|List1],[Y|List2],ErrorAcc)->
	list_compare(List1,List2,ErrorAcc+erlang:abs(X-Y));
list_compare([],[],ErrorAcc)->erlang:abs(ErrorAcc).