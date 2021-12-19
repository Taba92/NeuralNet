-module(trainer).
-export([fit/2]).
-include("utils.hrl").
-include("phenotype.hrl").
-include("genotype.hrl").

fit(State,Parameters)when map_get(type,Parameters)==shc->
	fit_shc(State,Parameters);
fit(State,Parameters)when map_get(type,Parameters)==ashc->
	#agent{genotype=Geno,fitness=Fit}=State,
	AlgoParameters=maps:merge(Parameters,#{bestGeno=>Geno,bestFit=>Fit}),
	fit_ashc(State,AlgoParameters);
fit(State,Parameters)when map_get(type,Parameters)==eshc->
	#agent{genotype=Geno,fitness=Fit}=State,
	AlgoParameters=maps:merge(Parameters,#{bestGeno=>Geno,bestFit=>Fit}),
	fit_eshc(State,AlgoParameters);
fit(State,Parameters)when map_get(type,Parameters)==som->
	AlgoParameters=Parameters#{iterations=>map_get(cycle,Parameters),fitness=>null},
	FittedState=fit_som(State,AlgoParameters),
	#{num_clusters:=K}=Parameters,
	KMeansParameters=AlgoParameters#{converged=>false,last_centroids=>get_random_centroids(FittedState,K)},
	k_means(FittedState,KMeansParameters).


%%%FIT ALGORITMHS FOR UNSUPERVISED LEARNING
fit_som(State,AlgoParameters)->%online update
	#agent{scape=Scape,genotype=Geno,cortex_id=CortexId}=State,
	#{cycle:=Cycle,iterations:=Iterations,learnRate:=LearnRate,neighboorSize:=NeighboorSize,fitness:=Fitness}=AlgoParameters,
	case Cycle==0 of
		true->
			FittedGeno=phenotype:pheno_to_geno(CortexId),
			State#agent{genotype=FittedGeno,fitness=Fitness};
		false->
			gen_server:call(Scape,reset),
			NeuronsIds=genotype:get_neurons_ids(Geno),
			CurLearnRate=LearnRate*(1-(Iterations-Cycle+1)/Iterations),
			CurNeighboorSize=NeighboorSize*(1-(Iterations-Cycle)/Iterations),
			NewFitness=learn_som(CortexId,NeuronsIds,CurLearnRate,CurNeighboorSize),
			NewParams=AlgoParameters#{cycle=>Cycle-1,fitness=>NewFitness},
			fit_som(State,NewParams)
	end.

learn_som(CortexId,Neurons,LearnRate,NeighboorSize)->
	gen_server:cast(CortexId,fit_cycle),
	receive {fit,Flag,Msg}->ok end,
	#{bmu:=BMU}=Msg,
	{_,Coord,_}=BMU,
	[gen_server:call(Id,{update_weight,LearnRate,{utils,gaussian_neighborhood,[Coord,NeighboorSize]}})||Id<-Neurons],
	case Flag of
		another->
			learn_som(CortexId,Neurons,LearnRate,NeighboorSize);
		finish->
			#{fitness:=Fit,loss:=Loss}=Msg,
			io:fwrite("FITNESS: ~p~n",[Fit]),
			io:fwrite("LOSS: ~p~n",[Loss]),
			Fit
	end.
%%%

k_means(State,AlgoParameters)->
	#{k_iterations:=Iterations,converged:=Converged,last_centroids:=LastCentroids}=AlgoParameters,
	case (Iterations==0) or (Converged==true) of
		true->State;
		false->
			{NewState,FinalCentroids}=assign_to_centroids(State,LastCentroids),
			case LastCentroids==FinalCentroids of
				true->
					NewAlgoParameters=AlgoParameters#{k_iterations=>Iterations-1,converged=>true},
					k_means(NewState,NewAlgoParameters);
				false->
					NewAlgoParameters=AlgoParameters#{k_iterations=>Iterations-1,last_centroids=>FinalCentroids},
					k_means(NewState,NewAlgoParameters)
			end
	end.

get_random_centroids(State,K)->
	#agent{genotype=Geno}=State,
	#genotype{neurons=Neurons}=Geno,
	Weights=[Weight||#neuron_som_genotype{weight=Weight}<-Neurons],
	get_random_centroids(Weights,K,[]).
get_random_centroids(_,K,Centroids)when K==length(Centroids)->Centroids;
get_random_centroids(Weights,K,Centroids)->
	Centroid=?RANDCHOOSE(Weights),
	get_random_centroids(Weights--[Centroid],K,Centroids++[Centroid]).

assign_to_centroids(State,Centroids)->
	#agent{cortex_id=CortexId}=State,
	phenotype:cluster_setting(CortexId,Centroids),
	ClusteredGeno=phenotype:pheno_to_geno(CortexId),
	ClusteredState=State#agent{genotype=ClusteredGeno},
	NewCentroids=get_centroids(ClusteredState,length(Centroids)),
	{ClusteredState,NewCentroids}.

get_centroids(State,K)->
	#agent{genotype=Geno}=State,
	#genotype{sensors=[Sensor],neurons=Neurons}=Geno,
	Clusters=[[Neuron||Neuron<-Neurons,Neuron#neuron_som_phenotype.cluster==C]||C<-lists:seq(1,K)],
	[weights_avg(Cluster,Sensor)||Cluster<-Clusters].

weights_avg(Cluster,Sensor)->
	CentroidLen=Sensor#sensor_genotype.signal_input_length,
	weights_avg(Cluster,length(Cluster),lists:duplicate(CentroidLen,0)).
weights_avg([],N,Acc)->[El/N||El<-Acc];
weights_avg([HNeuron|T],N,Acc)->
	#neuron_som_genotype{weight=Weight}=HNeuron,
	weights_avg(T,N,sum(Weight,Acc)).

sum(L1,L2)->[X+Y||{X,Y}<-lists:zip(L1,L2)].

%%FIT ALGORITHMS FOR SUPERVISED LEARNING
fit_eshc(State,AlgoParameters)->
	#{cycleEshc:=CycleEshc,mutations:=NMut,constraint:=Constraint,tgFit:=TgFit,bestGeno:=BestGeno,bestFit:=BestFit}=AlgoParameters,
	case (CycleEshc==0) or (BestFit>=TgFit) of
		true->
			State#agent{genotype=BestGeno,fitness=BestFit};
		false->
			FittedState=fit_shc(State,AlgoParameters),
			#agent{scape=Scape,genotype=FittedGeno,fitness=Fitness,cortex_id=CortexId}=FittedState,
			case Fitness > BestFit of
				true->
					NewParams=maps:merge(AlgoParameters,#{bestGeno=>FittedGeno,bestFit=>Fitness,cycleEshc=>CycleEshc-1}),
					fit_eshc(FittedState,NewParams);
				false->
					{NewGeno, _} = genotype_mutator:mutate(FittedGeno,NMut,Constraint),
					phenotype:stop_phenotype(CortexId),
					phenotype:geno_to_pheno(NewGeno),
					NewCortexId=genotype:get_cortex_id(NewGeno),
					NewState=phenotype:link_to_cortex(State,NewCortexId),
					phenotype:link_nn_to_scape(NewGeno,Scape),
					NewParams=maps:merge(AlgoParameters,#{cycleEshc=>CycleEshc-1}),
					fit_eshc(NewState#agent{genotype=NewGeno,fitness=0},NewParams)
			end
	end.

fit_ashc(State,AlgoParameters)->
	#{cycleAshc:=CycleAshc,constraint:=Constraint,tgFit:=TgFit,bestGeno:=BestGeno,bestFit:=BestFit}=AlgoParameters,
	case (CycleAshc==0) or (BestFit>=TgFit) of
		true->
			State#agent{genotype=BestGeno,fitness=BestFit};
		false->
			FittedState=fit_shc(State,AlgoParameters),
			#agent{scape=Scape,genotype=FittedGeno,fitness=Fitness,cortex_id=CortexId}=FittedState,
			case Fitness > BestFit of
				true->
					NewParams=maps:merge(AlgoParameters,#{bestGeno=>FittedGeno,bestFit=>Fitness,cycleAshc=>CycleAshc-1}),
					fit_ashc(FittedState,NewParams);
				false->
					{SensorSpec,ActuatorSpec,CortexSpec,HiddenLayers}=genotype:get_geno_spec(FittedGeno),
					NewGeno=genotype:create_NN(Constraint,SensorSpec,ActuatorSpec,CortexSpec,HiddenLayers),
					phenotype:stop_phenotype(CortexId),
					phenotype:geno_to_pheno(NewGeno),
					NewCortexId=genotype:get_cortex_id(NewGeno),
					NewState=phenotype:link_to_cortex(State,NewCortexId),
					phenotype:link_nn_to_scape(NewGeno,Scape),
					NewParams=maps:merge(AlgoParameters,#{cycleAshc=>CycleAshc-1}),
					fit_ashc(NewState#agent{genotype=NewGeno,fitness=0},NewParams)
			end
	end.

fit_shc(State,AlgoParameters)->
	#agent{scape=Scape,genotype=Geno,cortex_id=CortexId,fitness=CurFit}=State,
	#{stepnessNeuron:=StepN,stepnessWeight:=StepW,cycleShc:=CycleShc,tgFit:=TgFit}=AlgoParameters,
	#genotype{neurons=Neurons}=Geno,
	case (CycleShc==0) or (CurFit>=TgFit) of
		true->
			FittedGeno=phenotype:pheno_to_geno(CortexId),
			State#agent{genotype=FittedGeno};
		false->
			gen_server:call(Scape,reset),
			#{fitness:=NewFit} = nn_service:apply_to_scape(fit,CortexId),
			io:fwrite("FITNESS: ~p~n---------------------~n",[NewFit]),
			Prob=length(Neurons)*StepN/100,
			{NewState,NewParameters}=case NewFit >= CurFit of
						true->
							phenotype:backup_weights(CortexId),
							phenotype:perturb_weights({CortexId,Prob,StepW}),
							{State#agent{fitness=NewFit},maps:merge(AlgoParameters,#{cycleShc=>CycleShc-1})};
						false->
							phenotype:restore_weights(CortexId),
							phenotype:backup_weights(CortexId),
							phenotype:perturb_weights({CortexId,Prob,StepW}),
							{State,maps:merge(AlgoParameters,#{cycleShc=>CycleShc-1})}
				end,
			fit_shc(NewState,NewParameters)
	end.

