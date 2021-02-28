-module(neuron_som).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{received,roreceived,oldBias,oldWeights,oldRoWeights,histOut,histSig,genotype}).
-include("utils.hrl").

init(GenoType)when is_record(GenoType,neuron_som)->
	#neuron{id=Id}=GenoType,
	gen_server:start_link({local,Id},?MODULE,[GenoType],[]);
init([GenoType])->
	State=#state{genotype=GenoType},
	{ok,State}.

handle_call(dump,_,State)->
	#state{genotype=GenoType}=State,
	{reply,GenoType,State}.

terminate(normal,_)->ok.

handle_cast({ElType,_,_,FwdType,Signal},State)when ElType==sensor->
	#state{genotype=GenoType}=State,
	#neuron_som{id=Id,af=Af,weight=Weight,fanouts=Outs}=GenoType,
	Dist=af:Af(Weight,Signal),
	[gen_server:cast(Pid,{neuron,null,Id,FwdType,[Dist]})||Pid<-Outs],
	NewState=State#state{histOut={Dist},histSig={Signal}},
	{noreply,NewState}.

