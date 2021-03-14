-module(trainer).
-export([fit/2]).
-include("utils.hrl").

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
	AlgoParameters=maps:merge(Parameters,#{iterations=>map_get(cycle,Parameters)}),
	fit_som(State,AlgoParameters).


%%%FIT ALGORITMHS FOR UNSUPERVISED LEARNING
fit_som(State,AlgoParameters)->%online update
	#agent{scape=Scape,genotype=Geno,cortexId=CortexId}=State,
	#{cycle:=Cycle,iterations:=Iterations,learnRate:=LearnRate,neighboorSize:=NeighboorSize}=AlgoParameters,
	case Cycle==0 of
		true->
			FittedGeno=phenotype:pheno_to_geno(CortexId),
			State#agent{genotype=FittedGeno};
		false->
			gen_server:call(Scape,reset),
			NeuronsIds=genotype:get_neurons_ids(Geno),
			CurLearnRate=LearnRate*(1-(Iterations-Cycle+1)/Iterations),
			CurNeighboorSize=NeighboorSize*(1-(Iterations-Cycle)/Iterations),
			learn_som(CortexId,NeuronsIds,CurLearnRate,CurNeighboorSize),
			NewParams=maps:update(cycle,Cycle-1,AlgoParameters),
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
			%#{partial_fit:=Fit,partial_loss:=Loss}=Msg,
			%io:fwrite("PARTIAL FITNESS: ~p~n",[Fit]),
			%io:fwrite("PARTIAL LOSS: ~p~n",[Loss]),
			%timer:sleep(4000),
			learn_som(CortexId,Neurons,LearnRate,NeighboorSize);
		finish->
			#{fitness:=Fit,loss:=Loss}=Msg,
			io:fwrite("FITNESS: ~p~n",[Fit]),
			io:fwrite("LOSS: ~p~n",[Loss])
	end.
%%%


%%FIT ALGORITHMS FOR SUPERVISED LEARNING
fit_eshc(State,AlgoParameters)->
	#{cycleEshc:=CycleEshc,mutations:=NMut,constraint:=Constraint,tgFit:=TgFit,bestGeno:=BestGeno,bestFit:=BestFit}=AlgoParameters,
	case (CycleEshc==0) or (BestFit>=TgFit) of
		true->
			State#agent{genotype=BestGeno,fitness=BestFit};
		false->
			FittedState=fit_shc(State,AlgoParameters),
			#agent{scape=Scape,genotype=FittedGeno,fitness=Fitness,cortexId=CortexId}=FittedState,
			case Fitness > BestFit of
				true->
					NewParams=maps:merge(AlgoParameters,#{bestGeno=>FittedGeno,bestFit=>Fitness,cycleEshc=>CycleEshc-1}),
					fit_eshc(FittedState,NewParams);
				false->
					NewGeno=genotype_mutator:mutate(FittedGeno,NMut,Constraint),
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
			#agent{scape=Scape,genotype=FittedGeno,fitness=Fitness,cortexId=CortexId}=FittedState,
			case Fitness > BestFit of
				true->
					NewParams=maps:merge(AlgoParameters,#{bestGeno=>FittedGeno,bestFit=>Fitness,cycleAshc=>CycleAshc-1}),
					fit_ashc(FittedState,NewParams);
				false->
					{SensorSpec,ActuatorSpec,HiddenLayers}=genotype:get_geno_spec(FittedGeno),
					NewGeno=genotype:create_NN(Constraint,SensorSpec,ActuatorSpec,HiddenLayers),
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
	#agent{scape=Scape,genotype=Geno,cortexId=CortexId,fitness=CurFit}=State,
	#{stepnessNeuron:=StepN,stepnessWeight:=StepW,cycleShc:=CycleShc,tgFit:=TgFit}=AlgoParameters,
	#genotype{neurons=Neurons}=Geno,
	case (CycleShc==0) or (CurFit>=TgFit) of
		true->
			FittedGeno=phenotype:pheno_to_geno(CortexId),
			State#agent{genotype=FittedGeno};
		false->
			gen_server:call(Scape,reset),
			#{fitness:=NewFit}=utils:apply_to_scape(fit,CortexId),
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

