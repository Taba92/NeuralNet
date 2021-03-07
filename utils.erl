-module(utils).
-export([prob_on/1,get_id/0,randchoose/1,normalize_fit/1,perturbate/1,saturate/3,order/2]).
-export([apply_to_scape/2,actuator_get_signals/1,gaussian_neighborhood/3,get_BMU/1]).
-include("utils.hrl").

limit(Sup)->case round(math:floor(Sup)) of N when N<1->1;N->N end.

normalize_fit(Fit)->case is_number(Fit) of true->max(0,Fit);false->Fit end.
randchoose(List)->lists:nth(rand:uniform(length(List)),List).
get_id()->list_to_atom(integer_to_list(logger:timestamp()+erlang:unique_integer([positive,monotonic]))).
prob_on(Sup)->1==rand:uniform(limit(Sup)).

perturbate(Val)->
	NewVal=?RAND*?SAT_LIMIT+Val,
	saturate(NewVal,-?SAT_LIMIT,?SAT_LIMIT).

saturate(Val,Min,Max)->
	if
		Val < Min -> Min;
		Val > Max -> Max;
		true -> Val
	end.

order(List,TupleList)->
	order(List,TupleList,[]).
order([],_,Acc)->Acc;
order([H|T],TupleList,Acc)->
	{H,Value}=lists:keyfind(H,1,TupleList),
	order(T,TupleList,Acc++Value).


apply_to_scape(fit,CortexId)->
	gen_server:cast(CortexId,fit_cycle),
	receive
		{fit,another,_}->apply_to_scape(fit,CortexId);
		{fit,finish,Msg}->Msg
	end;
apply_to_scape(fit_predict,CortexId)->
	gen_server:cast(CortexId,fit_predict_cycle),
	receive
		{fit_predict,another,Msg}->
			io:fwrite("~p~n",[Msg]),
			apply_to_scape(fit_predict,CortexId);
		{fit_predict,finish,Msg}->io:fwrite("~p~n",[Msg])
	end.

%%SOM FUNCTIONS UTILS
actuator_get_signals(TupleList)->[Value||{_,_,[Value]}<-TupleList].
gaussian_neighborhood(X,Y,NeighBoorSize)->math:pow(?E,-(af:euclidean(X,Y)/2*math:pow(NeighBoorSize,2))).
get_BMU(TupleList)when is_list(TupleList)->erlang:hd(lists:keysort(3,TupleList)).
%%%