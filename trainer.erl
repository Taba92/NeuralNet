-module(trainer).
-export([fit/2]).
-include("utils.hrl").

fit(State,Parameters)when map_get(type,Parameters)==shc->
	#agent{genotype=Geno,fitness=Fit}=State,
	AlgoParameters=maps:merge(Parameters,#{curGeno=>Geno,curFit=>Fit}),
	fit_shc(State,AlgoParameters);
fit(State,Parameters)when map_get(type,Parameters)==ashc->
	#agent{genotype=Geno,fitness=Fit}=State,
	AlgoParameters=maps:merge(Parameters,#{curGeno=>null,curFit=>null,bestGeno=>Geno,bestFit=>Fit}),
	fit_ashc(State,AlgoParameters).

fit_ashc(State,AlgoParameters)->
	#agent{scape=Scape,genotype=Genotype,cortexId=CortexId}=State,
	#{cycleAshc:=CycleAshc,constraint:=Constraint,tgFit:=TgFit,bestGeno:=BestGeno,bestFit:=BestFit}=AlgoParameters,
	case (CycleAshc==0) or (BestFit>=TgFit) of
		true->
			NewState=State#agent{genotype=BestGeno,fitness=BestFit},
			{NewState,BestGeno,BestFit};
		false->
			{SVl,AVl,HiddenLayers}=genotype:get_geno_spec(Genotype),
			NewGeno=genotype:create_NN(Constraint,SVl,AVl,HiddenLayers),
			phenotype:stop_phenotype(CortexId),
			phenotype:geno_to_pheno(NewGeno),
			NewCortexId=genotype:get_cortex_id(NewGeno),
			NewState=phenotype:link_to_cortex(State,NewCortexId),
			phenotype:link_nn_to_scape(NewGeno,Scape),
			NewAlgoParameters=maps:merge(AlgoParameters,#{curGeno=>NewGeno,curFit=>0}),
			{_,FittedNewGeno,NewFitness}=fit_shc(NewState,NewAlgoParameters),
			case NewFitness > BestFit of
				true->
					NewParams=maps:merge(NewAlgoParameters,#{bestGeno=>FittedNewGeno,bestFit=>NewFitness,cycleAshc=>CycleAshc-1}),
					fit_ashc(NewState,NewParams);
				false->
					phenotype:geno_to_pheno(BestGeno),
					NewState=phenotype:link_to_cortex(State,NewCortexId),
					phenotype:link_nn_to_scape(BestGeno,Scape),
					NewParams=maps:merge(NewAlgoParameters,#{cycleAshc=>CycleAshc-1}),
					fit_ashc(State,NewParams)
			end
	end.

fit_shc(State,AlgoParameters)->
	#agent{cortexId=CortexId}=State,
	#{curGeno:=Geno,curFit:=CurFit,cycleShc:=CycleShc,tgFit:=TgFit}=AlgoParameters,
	#genotype{neurons=Neurons}=Geno,
	case (CycleShc==0) or (CurFit>=TgFit) of
		true->
			FittedGeno=phenotype:pheno_to_geno(CortexId),
			NewState=State#agent{genotype=FittedGeno,fitness=CurFit},
			{NewState,FittedGeno,CurFit};
		false->
			CortexId ! fit_cycle,
			receive {fitness,NewFit}->ok end,
			NewParameters=case NewFit >= CurFit of
						true->
							phenotype:backup_weights(CortexId),
							phenotype:perturb_weights({CortexId,length(Neurons)}),
							maps:merge(AlgoParameters,#{curFit=>NewFit,cycleShc=>CycleShc-1});
						false->
							phenotype:restore_weights(CortexId),
							phenotype:backup_weights(CortexId),
							phenotype:perturb_weights({CortexId,length(Neurons)}),
							maps:merge(AlgoParameters,#{cycleShc=>CycleShc-1})
				end,
			fit_shc(State,NewParameters)
	end.