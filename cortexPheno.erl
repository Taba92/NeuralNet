-module(cortexPheno).
-export([init/1,terminate/2]).
-export([handle_info/2,handle_call/3,handle_cast/2]).
-record(state,{controllerId,received,genotype}).
-include("utils.hrl").

init(GenoType)when is_record(GenoType,cortex)->
	#cortex{id=Id}=GenoType,
	gen_server:start_link({local,Id},?MODULE,[GenoType],[]);
init([GenoType])->
	State=#state{received=[],genotype=GenoType},
	{ok,State}.

terminate(normal,_)->ok.

handle_call(dump,_,State)->
	#state{genotype=GenoType}=State,
	{reply,GenoType,State};
handle_call({controller_id,Id},_,State)->
	{reply,ok,State#state{controllerId=Id}}.
	
handle_cast(fit_cycle,State)->
	#state{genotype=GenoType}=State,
	#cortex{sensorsIds=SensorsIds}=GenoType,
	[gen_server:cast(Sensor,sync_fit)||Sensor<-SensorsIds],
	{noreply,State};
handle_cast({predict_cycle,Signal},State)->
	#state{genotype=GenoType}=State,
	#cortex{sensorsIds=SensorsIds}=GenoType,
	[gen_server:cast(Sensor,{sync_predict,Signal})||Sensor<-SensorsIds],
	{noreply,State};
handle_cast(fit_predict_cycle,State)->
	#state{genotype=GenoType}=State,
	#cortex{sensorsIds=SensorsIds}=GenoType,
	[gen_server:cast(Sensor,sync_fit_predict)||Sensor<-SensorsIds],
	{noreply,State}.

handle_info({fit,Id,Flag,Msg},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex{fit_directives=Funs,actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv=Recv++[{Id,Msg}],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					OrderedMsgs=order(ActuatorsIds,NewRecv),
					ProcessedMsgs=eval_funs(OrderedMsgs,Funs),
					Control ! {fit,Flag,ProcessedMsgs},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_info({fit_predict,Id,Flag,Msg},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex{real_directives=Funs,actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv=Recv++[{Id,Msg}],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					OrderedMsgs=order(ActuatorsIds,NewRecv),
					ProcessedMsgs=eval_funs(OrderedMsgs,Funs),
					Control ! {fit_predict,Flag,ProcessedMsgs},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_info({predict,Id,Pred},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex{fit_directives=Funs,actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv=Recv++[{Id,Pred}],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					OrderedMsgs=order(ActuatorsIds,NewRecv),
					ProcessedMsgs=eval_funs(OrderedMsgs,Funs),
					Control ! {prediction,ProcessedMsgs},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState}.

order(List,TupleList)->
	order(List,TupleList,[]).
order([],_,Acc)->Acc;
order([H|T],TupleList,Acc)->
	{H,Value}=lists:keyfind(H,1,TupleList),
	order(T,TupleList,Acc++[Value]).

eval_funs(Signal,[])->Signal;
eval_funs(Signal,[{Mod,Fun,ExtraArgs}|T])when is_atom(Mod),is_atom(Fun),is_list(ExtraArgs)->
	NewSignal=erlang:apply(Mod,Fun,[Signal|ExtraArgs]),
	eval_funs(NewSignal,T);
eval_funs(Signal,[{Fun,ExtraArgs}|T])when is_function(Fun),is_list(ExtraArgs)->
	NewSignal=erlang:apply(Fun,[Signal|ExtraArgs]),
	eval_funs(NewSignal,T).