-module(cortex).
-export([init/1,terminate/2]).
-export([handle_call/3,handle_cast/2]).
-record(state,{controllerId,received,genotype}).
-include("utils.hrl").
-include("phenotype.hrl").

init(GenoType)when is_record(GenoType,cortex_phenotype)->
	#cortex_phenotype{id=Id}=GenoType,
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
	#cortex_phenotype{sensorsIds=SensorsIds}=GenoType,
	[gen_server:cast(Sensor,sync_fit)||Sensor<-SensorsIds],
	{noreply,State};
handle_cast({predict_cycle,Signal},State)->
	#state{genotype=GenoType}=State,
	#cortex_phenotype{sensorsIds=SensorsIds}=GenoType,
	[gen_server:cast(Sensor,{sync_predict,Signal})||Sensor<-SensorsIds],
	{noreply,State};
handle_cast(fit_predict_cycle,State)->
	#state{genotype=GenoType}=State,
	#cortex_phenotype{sensorsIds=SensorsIds}=GenoType,
	[gen_server:cast(Sensor,sync_fit_predict)||Sensor<-SensorsIds],
	{noreply,State};
handle_cast({fit,Id,Flag,Msg},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex_phenotype{fit_directives=Funs,actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv = Recv ++ [{Id, null, Msg}],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					OrderedMsgs = nn_service:order_by_keylist(ActuatorsIds, NewRecv),
					Msgs = [Message || {_, _, Message} <- OrderedMsgs],
					%%Functions pipes are functions that takes a vector of msgs(list of maps, see scapes return)
					ProcessedMsgs=nn_service:apply_directives_pipe(Msgs,Funs),
					Control ! {fit,Flag,ProcessedMsgs},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_cast({fit_predict,Id,Flag,Msg},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex_phenotype{real_directives=Funs,actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv = Recv ++ [{Id, null, Msg}],
	NewState=case length(NewRecv)==length(ActuatorsIds) of
				true->
					OrderedMsgs = nn_service:order_by_keylist(ActuatorsIds, NewRecv),
					Msgs = [Message || {_, _, Message} <- OrderedMsgs],
					%%Functions pipes are functions that takes a vector of msgs(list of maps, see scapes return)
					ProcessedMsgs = nn_service:apply_directives_pipe(Msgs,Funs),
					Control ! {fit_predict,Flag,ProcessedMsgs},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_cast({predict,Id,Pred},State)->
	#state{controllerId=Control,received=Recv,genotype=GenoType}=State,
	#cortex_phenotype{fit_directives=Funs,actuatorsIds=ActuatorsIds}=GenoType,
	NewRecv = Recv ++ [{Id, null, Pred}],
	NewState=case length(NewRecv) == length(ActuatorsIds) of
				true->
					OrderedMsgs = nn_service:order_by_keylist(ActuatorsIds, NewRecv),
					Msgs = [Message || {_, _, Message} <- OrderedMsgs],
					%%Functions pipes are functions that takes a vector of msgs(list of maps, see scapes return)
					ProcessedMsgs=nn_service:apply_directives_pipe(Msgs,Funs),
					io:fwrite("PREDICT: ~p~n",[ProcessedMsgs]),
					Control ! {prediction,ProcessedMsgs},
					State#state{received=[]};
				false->State#state{received=NewRecv}
			end,
	{noreply,NewState}.
