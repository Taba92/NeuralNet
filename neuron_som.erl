-module(neuron_som).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{received,roreceived,oldBias,oldWeights,oldRoWeights,histOut,histSig,genotype}).
-include("utils.hrl").

init(GenoType)when is_record(GenoType,neuron_som)->
	#neuron_som{id=Id}=GenoType,
	gen_server:start_link({local,Id},?MODULE,[GenoType],[]);
init([GenoType])->
	State=#state{genotype=GenoType},
	{ok,State}.

handle_call(dump,_,State)->
	#state{genotype=GenoType}=State,
	{reply,GenoType,State};
handle_call({update_weight,LearnRate,{Mod,NeighboorFun,PartialArgs}},_,State)->
	#state{histSig={_,Signal},genotype=GenoType}=State,
	#neuron_som{coordinates=Coord,weight=Weight}=GenoType,
	Scalar=LearnRate*erlang:apply(Mod,NeighboorFun,[Coord|PartialArgs]),
	SubVect=[X-Y||{X,Y}<-lists:zip(Signal,Weight)],
	DeltaVect=[X*Y||X<-[Scalar],Y<-SubVect],
	NewWeight=[X+Y||{X,Y}<-lists:zip(Weight,DeltaVect)],
	NewGenoType=GenoType#neuron_som{weight=NewWeight},
	NewState=State#state{genotype=NewGenoType},
	{reply,ok,NewState}.

terminate(normal,_)->ok.

handle_cast({ElType,_,IdFrom,FwdType,Signal},State)when ElType==sensor->
	#state{genotype=GenoType}=State,
	#neuron_som{id=Id,coordinates=Coord,af=Af,weight=Weight,fanouts=Outs}=GenoType,
	Dist=af:Af(Weight,Signal),
	[gen_server:cast(Pid,{neuron,Coord,Id,FwdType,[Dist]})||Pid<-Outs],
	NewState=State#state{histOut={Dist},histSig={IdFrom,Signal}},
	{noreply,NewState}.

