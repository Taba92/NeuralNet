-module(utils).
-export([prob_on/1,get_id/0,randchoose/1,normalize_fit/1,perturbate/1,saturate/3]).
-export([apply_to_scape/2]).
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

apply_to_scape(fit,CortexId)->
	gen_server:cast(CortexId,fit_cycle),
	receive
		{fit_another,_}->apply_to_scape(fit,CortexId);
		{fit_finish,Msg}->Msg
	end;
apply_to_scape(fit_predict,CortexId)->
	gen_server:cast(CortexId,fit_predict_cycle),
	receive
		{fit_predict_another,_}->apply_to_scape(fit_predict,CortexId);
		{fit_predict_finish,Msg}->Msg
	end.