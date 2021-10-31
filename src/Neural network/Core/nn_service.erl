-module(nn_service).
-export([apply_directives_pipe/2, order_by_keylist/2, apply_to_scape/2, perturbate/1]).
-include("utils.hrl").

%General services for a neural network

%%Given an input and list of function, create an evaluation pipe on the input
apply_directives_pipe(Signal,[])->Signal;
apply_directives_pipe(Signal,[{Mod,Fun,ExtraArgs}|T])when is_atom(Mod),is_atom(Fun),is_list(ExtraArgs)->
	NewSignal=erlang:apply(Mod,Fun,[Signal|ExtraArgs]),
	apply_directives_pipe(NewSignal,T);
apply_directives_pipe(Signal,[{Fun,ExtraArgs}|T])when is_function(Fun),is_list(ExtraArgs)->
	NewSignal=erlang:apply(Fun,[Signal|ExtraArgs]),
	apply_directives_pipe(NewSignal,T).

%Given a list of keys and a tuple list, order the TupleList in the order of the keys of SortList
order_by_keylist(SortList,TupleListToOrder)->
	order_by_keylist(SortList,TupleListToOrder,[]).
order_by_keylist([],_,Acc)->Acc;
order_by_keylist([H|T],TupleListToOrder,Acc)->
	{H,Term,Value}=lists:keyfind(H,1,TupleListToOrder),
	order_by_keylist(T,TupleListToOrder,Acc++[{H,Term,Value}]).

%%Perform a cycle inside the NN given an instruction( fit, fit_predict)
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

%%Given a value Val, it perturb the value and return the perturbed val NewVal.
perturbate(Val)->
	NewVal = ?RAND * ?SAT_LIMIT + Val,
	math_utils:saturate(NewVal, -?SAT_LIMIT, ?SAT_LIMIT).