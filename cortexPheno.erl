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

handle_info({fit,Id,finish,Msg},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex{actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv=Recv++[Id],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					#{fitness:=Fitness}=Msg,
					io:fwrite("FITNESS: ~p~n",[Fitness]),
					io:fwrite("---------------------------------~n"),
					Control ! {fit_finish,Msg},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_info({fit,Id,another,Msg},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex{actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv=Recv++[Id],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					Control ! {fit_another,Msg},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_info({fit_predict,Id,finish,Msg},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex{actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv=Recv++[Id],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					#{target:=Target,predict:=Predict}=Msg,
					io:fwrite("VALUE PREDICT: ~p TRUE VALUE: ~p~n",[Predict,Target]),
					Control ! {fit_predict_finish,Msg},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_info({fit_predict,Id,another,Msg},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex{actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv=Recv++[Id],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					#{target:=Target,predict:=Predict}=Msg,
					io:fwrite("VALUE PREDICT: ~p TRUE VALUE: ~p~n",[Predict,Target]),
					Control ! {fit_predict_another,Msg},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_info({predict,Id,Pred},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex{actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv=Recv++[Id],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					Control ! {prediction,Pred},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState}.