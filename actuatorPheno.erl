-module(actuatorPheno).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{scapeId,received,genotype}).
-include("utils.hrl").

init(GenoType)when is_record(GenoType,actuator)->
	#actuator{id=Id}=GenoType,
	gen_server:start_link({local,Id},?MODULE,[GenoType],[]);
init([GenoType])->
	State=#state{received=[],genotype=GenoType},
	{ok,State}.

handle_call(dump,_,State)->
	#state{genotype=GenoType}=State,
	{reply,GenoType,State};
handle_call({set_scape,Scape},_,State)->
	{reply,ok,State#state{scapeId=Scape}}.


terminate(normal,_)->ok.

handle_cast({neuron,Term,NId,forward_fit,Signal},State)->
	#state{scapeId=Scape,received=Recv,genotype=GenoType}=State,
	#actuator{id=Id,vl=Vl,fit_directives=Funs,fanins=Ins,cortexId=CortexId}=GenoType,
	NewRecv=Recv++[{NId,Term,Signal}],
	NewState=case length(NewRecv)==Vl of
				true->
					OrderedSignal=order(Ins,NewRecv),
					ProcessedSignal=eval_funs(OrderedSignal,Funs),
					io:fwrite("SIGNAL: ~p~n",[ProcessedSignal]),
					{Flag,Msg}=gen_server:call(Scape,{action_fit,ProcessedSignal},infinity),
					gen_server:cast(CortexId,{fit,Id,Flag,Msg}),
					State#state{received=[]};
				false->
					State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_cast({neuron,Term,NId,forward_fit_predict,Signal},State)->
	#state{scapeId=Scape,received=Recv,genotype=GenoType}=State,
	#actuator{id=Id,vl=Vl,real_directives=Funs,fanins=Ins,cortexId=CortexId}=GenoType,
	NewRecv=Recv++[{NId,Term,Signal}],
	NewState=case length(NewRecv)==Vl of
				true->
					OrderedSignal=order(Ins,NewRecv),
					ProcessedSignal=eval_funs(OrderedSignal,Funs),
					{Flag,Msg}=gen_server:call(Scape,{action_fit_predict,ProcessedSignal},infinity),
					gen_server:cast(CortexId,{fit_predict,Id,Flag,Msg}),
					State#state{received=[]};
				false->
					State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_cast({neuron,Term,NId,forward_predict,Signal},State)->
	#state{scapeId=Scape,received=Recv,genotype=GenoType}=State,
	#actuator{id=Id,vl=Vl,real_directives=Funs,fanins=Ins,cortexId=CortexId}=GenoType,
	NewRecv=Recv++[{NId,Term,Signal}],
	NewState=case length(NewRecv)==Vl of
				true->
					OrderedSignal=order(Ins,NewRecv),
					ProcessedPred=eval_funs(OrderedSignal,Funs),
					gen_server:call(Scape,{action_predict,ProcessedPred},infinity),
					gen_server:cast(CortexId,{predict,Id,ProcessedPred}),
					State#state{received=[]};
				false->
					State#state{received=NewRecv}
			end,
	{noreply,NewState}.

order(SortList,TupleListToOrder)->
	order(SortList,TupleListToOrder,[]).
order([],_,Acc)->Acc;
order([H|T],TupleListToOrder,Acc)->
	{H,Term,Value}=lists:keyfind(H,1,TupleListToOrder),
	order(T,TupleListToOrder,Acc++[{H,Term,Value}]).


%%FUNCTIONS USED TO POSTPROCESS SIGNAL MUST TAKE THE SIGNAL VECTOR(IS A LIST) AS FIRST ARGUMENT!!!
eval_funs(Signal,[])->Signal;
eval_funs(Signal,[{Mod,Fun,ExtraArgs}|T])when is_atom(Mod),is_atom(Fun),is_list(ExtraArgs)->
	NewSignal=erlang:apply(Mod,Fun,[Signal|ExtraArgs]),
	eval_funs(NewSignal,T);
eval_funs(Signal,[{Fun,ExtraArgs}|T])when is_function(Fun),is_list(ExtraArgs)->
	NewSignal=erlang:apply(Fun,[Signal|ExtraArgs]),
	eval_funs(NewSignal,T).