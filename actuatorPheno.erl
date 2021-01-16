-module(actuatorPheno).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{scapeId,received,genotype}).
-define(NORMFIT(Fit),max(0,Fit)).
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

handle_cast({neuron,_,_,forward_fit,Signal},State)->
	#state{scapeId=Scape,received=Recv,genotype=GenoType}=State,
	#actuator{id=Id,vl=Vl,cortexId=CortexId}=GenoType,
	NewRecv=Recv++Signal,
	%io:fwrite("SIGNAL: ~p~n",[Signal]),
	NewState=case length(NewRecv)==Vl of
				true->
					{Ret,Flag}=gen_server:call(Scape,{action_fit,NewRecv},infinity),
					CortexId ! {fit,Id,?NORMFIT(Ret),Flag},
					State#state{received=[]};
				false->
					State#state{received=NewRecv}
			end,
	{noreply,NewState};
handle_cast({neuron,_,_,forward_predict,Signal},State)->
	#state{received=Recv,genotype=GenoType}=State,
	#actuator{id=Id,vl=Vl,cortexId=CortexId}=GenoType,
	NewRecv=Recv++Signal,
	%io:fwrite("SIGNAL: ~p~n",[Signal]),
	NewState=case length(NewRecv)==Vl of
				true->
					CortexId ! {predict,Id,NewRecv},
					State#state{received=[]};
				false->
					State#state{received=NewRecv}
			end,
	{noreply,NewState}.