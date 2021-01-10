-module(cortexPheno).
-export([init/1,terminate/2]).
-export([handle_info/2,handle_call/3]).
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
	
handle_info(fit_cycle,State)->
	#state{genotype=GenoType}=State,
	#cortex{sensorsIds=SensorsIds}=GenoType,
	Sync=fun(Sensor)->gen_server:cast(Sensor,sync_fit) end,
	%Sync=fun(Sensor)->Sensor ! sync_fit end,
	lists:foreach(Sync,SensorsIds),
	{noreply,State};
handle_info({predict_cycle,Signal},State)->
	#state{genotype=GenoType}=State,
	#cortex{sensorsIds=SensorsIds}=GenoType,
	Sync=fun(Sensor)->Sensor ! {sync_predict,Signal} end,
	%Sync=fun(Sensor)->Sensor ! {sync_predict,Signal} end,
	lists:foreach(Sync,SensorsIds),
	{noreply,State};
handle_info({fit,Id,Fitness,1},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex{actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv=Recv++[Id],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					io:fwrite("FITNESS: ~p~n",[Fitness]),
					io:fwrite("---------------------------------~n"),
					Control ! {fitness,Fitness},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_info({fit,Id,_,0},State)->
	#state{received=Recv,genotype=GenoType}=State,
	#cortex{sensorsIds=SensorsIds,actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv=Recv++[Id],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					Sync=fun(Sensor)->gen_server:cast(Sensor,sync_fit) end,
					%Sync=fun(Sensor)->Sensor ! sync_fit end,
					lists:foreach(Sync,SensorsIds),
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