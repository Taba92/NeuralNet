-module(actuator).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{scapeId,received,genotype}).
-include("utils.hrl").
-include("phenotype.hrl").

init(GenoType)when is_record(GenoType,actuator_phenotype)->
	#actuator_phenotype{id=Id}=GenoType,
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
	#actuator_phenotype{id=Id,vl=Vl,fit_directives=Funs,fanins=Ins,cortex_id=CortexId}=GenoType,
	NewRecv=Recv++[{NId,Term,Signal}],
	NewState=case length(NewRecv)==Vl of
				true->
					OrderedSignal = nn_service:order_by_keylist(Ins, NewRecv),
					% Function pipes are function that take a vector of numbers
					ProcessedSignal = nn_service:apply_directives_pipe(OrderedSignal, Funs),
					%io:fwrite("SIGNAL: ~p~n",[utils:get_BMU(ProcessedSignal)]),
					{Flag,Msg}=gen_server:call(Scape,{action_fit,ProcessedSignal},infinity),
					gen_server:cast(CortexId,{fit,Id,Flag,Msg}),
					State#state{received=[]};
				false->
					State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_cast({neuron,Term,NId,forward_fit_predict,Signal},State)->
	#state{scapeId=Scape,received=Recv,genotype=GenoType}=State,
	#actuator_phenotype{id=Id,vl=Vl,real_directives=Funs,fanins=Ins,cortex_id=CortexId}=GenoType,
	NewRecv=Recv++[{NId,Term,Signal}],
	NewState=case length(NewRecv)==Vl of
				true->
					OrderedSignal = nn_service:order_by_keylist(Ins, NewRecv),
					% Function pipes are function that take a vector of numbers
					ProcessedSignal = nn_service:apply_directives_pipe(OrderedSignal, Funs),
					{Flag,Msg}=gen_server:call(Scape,{action_fit_predict,ProcessedSignal},infinity),
					gen_server:cast(CortexId,{fit_predict,Id,Flag,Msg}),
					State#state{received=[]};
				false->
					State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_cast({neuron,Term,NId,forward_predict,Signal},State)->
	#state{scapeId=Scape,received=Recv,genotype=GenoType}=State,
	#actuator_phenotype{id=Id,vl=Vl,real_directives=Funs,fanins=Ins,cortex_id=CortexId}=GenoType,
	NewRecv=Recv++[{NId,Term,Signal}],
	NewState=case length(NewRecv)==Vl of
				true->
					OrderedSignal = nn_service:order_by_keylist(Ins, NewRecv),
					% Function pipes are function that take a vector of numbers
					ProcessedPred = nn_service:apply_directives_pipe(OrderedSignal, Funs),
					gen_server:call(Scape,{action_predict,ProcessedPred},infinity),
					gen_server:cast(CortexId,{predict,Id,ProcessedPred}),
					State#state{received=[]};
				false->
					State#state{received=NewRecv}
			end,
	{noreply,NewState}.

