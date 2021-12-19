-module(neuron_som).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{lastOutput,lastSignals,genotype}).
-include("utils.hrl").
-include("phenotype.hrl").

init(GenoType)when is_record(GenoType,neuron_som_phenotype)->
	#neuron_som_phenotype{id = Id}=GenoType,
	gen_server:start_link({local,Id},?MODULE,[GenoType],[]);
init([GenoType])->
	State=#state{genotype=GenoType},
	{ok,State}.

handle_call(dump,_,State)->
	#state{genotype=GenoType}=State,
	{reply,GenoType,State};
handle_call({update_weight,LearnRate,{Mod,NeighboorFun,PartialArgs}},_,State)->
	#state{lastSignals=Signal,genotype=GenoType}=State,
	#neuron_som_phenotype{coordinates=Coord,weight=Weight}=GenoType,
	Scalar=LearnRate*erlang:apply(Mod,NeighboorFun,[Coord|PartialArgs]),
	SubVect=[X-Y||{X,Y}<-lists:zip(Signal,Weight)],
	DeltaVect=[X*Y||X<-[Scalar],Y<-SubVect],
	NewWeight=[X+Y||{X,Y}<-lists:zip(Weight,DeltaVect)],
	NewGenoType=GenoType#neuron_som_phenotype{weight=NewWeight},
	NewState=State#state{genotype=NewGenoType},
	{reply,ok,NewState};
handle_call(get_neighbors,_,State)->
	#state{genotype=GenoType}=State,
	#neuron_som_phenotype{neighbors=Neighboors}=GenoType,
	{reply,Neighboors,State};
handle_call({cluster_setting,Centroids},_,State)->
	#state{genotype=GenoType}=State,
	#neuron_som_phenotype{af=Af,weight=Weight}=GenoType,
	Dists=[af:Af(Weight,Centroid)||Centroid<-Centroids],
	MinDist=lists:min(Dists),
	ClusterMap=lists:zip(lists:seq(1,length(Centroids)),Dists),
	{NumCluster,_}=lists:keyfind(MinDist,2,ClusterMap),
	NewGenoType=GenoType#neuron_som_phenotype{cluster=NumCluster},
	{reply,ok,State#state{genotype=NewGenoType}}.

terminate(normal,_)->ok.

handle_cast({ElType,_,_,forward_fit,Signal},State)when ElType==sensor->
	#state{genotype=GenoType}=State,
	#neuron_som_phenotype{id=Id,coordinates=Coord,af=Af,weight=Weight,fanouts=Outs}=GenoType,
	Dist=af:Af(Weight,Signal),
	[gen_server:cast(Pid,{neuron,Coord,Id,forward_fit,[Dist]})||Pid<-Outs],
	NewState=State#state{lastOutput=Dist,lastSignals=Signal},
	{noreply,NewState};
handle_cast({ElType,_,_,FwdType,Signal},State)when ElType==sensor->
	#state{genotype=GenoType}=State,
	#neuron_som_phenotype{id=Id,coordinates=Coord,af=Af,weight=Weight,cluster=Cluster,fanouts=Outs}=GenoType,
	Dist=af:Af(Weight,Signal),
	[gen_server:cast(Pid,{neuron,{Coord,Cluster},Id,FwdType,[Dist]})||Pid<-Outs],
	NewState=State#state{lastOutput=Dist,lastSignals=Signal},
	{noreply,NewState}.