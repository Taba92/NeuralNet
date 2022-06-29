-module(trainer).
-export([fit/2]).
-include("utils.hrl").
-include("phenotype.hrl").
-include("genotype.hrl").
-include_lib("common_test/include/ct.hrl").

fit(Agent, Parameters) when map_get(type, Parameters) == shc ->
	fit_shc(Agent, Parameters);
fit(Agent, Parameters) when map_get(type, Parameters) == eshc ->
	#agent{phenotype = Phenotype, fitness = Fit} = Agent,
	Genotype = ?NN_SERVICE_MODULE:phenotype_to_genotype(Phenotype),
	AlgoParameters = maps:merge(Parameters, #{bestGenotype => Genotype, bestFit => Fit}),
	fit_eshc(Agent, AlgoParameters);
fit(Agent, Parameters) when map_get(type, Parameters) == som ->
	AlgoParameters = Parameters#{iterations => map_get(cycle, Parameters), fitness => null},
	FittedState = fit_som(Agent, AlgoParameters),
	#{num_clusters := K} = Parameters,
	KMeansParameters = AlgoParameters#{converged => false, last_centroids => get_random_centroids(FittedState, K)},
	k_means(FittedState, KMeansParameters).


%%%FIT ALGORITMHS FOR UNSUPERVISED LEARNING
fit_som(Agent, AlgoParameters) -> %online update
	#agent{scape_id = ScapeId, phenotype = Phenotype} = Agent,
	#{cycle := Cycle, iterations := Iterations, learnRate := LearnRate, neighboorSize := NeighboorSize, fitness := Fitness} = AlgoParameters,
	case Cycle == 0 of
		true->
			Agent#agent{fitness = Fitness};
		false->
			gen_server:call(ScapeId, reset),
			CurLearnRate = LearnRate * (1 - (Iterations - Cycle + 1) / Iterations),
			CurNeighboorSize = NeighboorSize * (1 - (Iterations - Cycle) / Iterations),
			NewFitness = learn_som(Phenotype, CurLearnRate, CurNeighboorSize),
			NewParams = AlgoParameters#{cycle => Cycle - 1, fitness => NewFitness},
			fit_som(Agent, NewParams)
	end.

learn_som(Phenotype, LearnRate, NeighboorSize) ->
	CortexId = phenotype:get_cortex_id(Phenotype),
	NeuronsIds = phenotype:get_neurons_ids(Phenotype),
	gen_server:cast(CortexId, fit_cycle),
	receive 
		{fit, Flag, Msg} -> ok 
	end,
	#{bmu := BMU} = Msg,
	{_, Coord, _} = BMU,
	[gen_server:call(Id, {update_weight, LearnRate, {utils,gaussian_neighborhood, [Coord, NeighboorSize]}}) || Id <- NeuronsIds],
	case Flag of
		another->
			learn_som(Phenotype, LearnRate,NeighboorSize);
		finish->
			#{fitness := Fit, loss := Loss} = Msg,
			io:format("FITNESS: ~p~n",[Fit]),
			io:format("LOSS: ~p~n",[Loss]),
			Fit
	end.
%%%

k_means(Agent, AlgoParameters) ->
	#{k_iterations := Iterations, converged := Converged, last_centroids := LastCentroids} = AlgoParameters,
	case (Iterations == 0) or (Converged == true) of
		true -> 
			Agent;
		false ->
			{NewAgent, FinalCentroids} = assign_to_centroids(Agent, LastCentroids),
			case LastCentroids == FinalCentroids of
				true ->
					NewAlgoParameters = AlgoParameters#{k_iterations => Iterations -1 ,converged => true},
					k_means(NewAgent, NewAlgoParameters);
				false->
					NewAlgoParameters = AlgoParameters#{k_iterations => Iterations - 1,last_centroids => FinalCentroids},
					k_means(NewAgent, NewAlgoParameters)
			end
	end.

get_random_centroids(Agent, K) ->
	#agent{phenotype = Phenotype} = Agent,
	Neurons = phenotype:get_neurons(Phenotype),
	Weights = [Weight || #neuron_som_phenotype{weight = Weight} <- Neurons],
	get_random_centroids(Weights, K, []).
get_random_centroids(_, K, Centroids) when K == length(Centroids) ->
	Centroids;
get_random_centroids(Weights, K, Centroids) ->
	Centroid = ?RANDCHOOSE(Weights),
	get_random_centroids(Weights -- [Centroid], K, Centroids ++ [Centroid]).

assign_to_centroids(Agent, Centroids)->
	ClusteredAgent = ?NN_SERVICE_MODULE:cluster_setting(Agent),
	NewCentroids = get_centroids(ClusteredAgent, length(Centroids)),
	{ClusteredAgent, NewCentroids}.

get_centroids(Agent, K) ->
	#agent{phenotype = Phenotype} = Agent,
	[Sensor] = phenotype:get_sensor(Phenotype),
	Neurons = phenotype:get_neurons(Phenotype),
	Clusters= [ [Neuron || Neuron <- Neurons, Neuron#neuron_som_phenotype.cluster == C] || C <- lists:seq(1, K)],
	[weights_avg(Cluster, Sensor) || Cluster <- Clusters].

weights_avg(Cluster,Sensor)->
	CentroidLen = Sensor#sensor_phenotype.signal_input_length,
	weights_avg(Cluster, length(Cluster), lists:duplicate(CentroidLen, 0)).
weights_avg([], N, Acc) ->
	[El / N || El <- Acc];
weights_avg([HNeuron | T], N, Acc) ->
	#neuron_som_phenotype{weight = Weight} = HNeuron,
	weights_avg(T, N, sum(Weight, Acc)).

sum(L1, L2) -> 
	[X + Y || {X, Y} <- lists:zip(L1, L2)].

%%FIT ALGORITHMS FOR SUPERVISED LEARNING
fit_eshc(Agent, AlgoParameters) ->
	#{cycleEshc := CycleEshc, mutations := NMut, constraint := Constraint, tgFit := TgFit, bestGeno := BestGenotype, bestFit := BestFit} = AlgoParameters,
	case (CycleEshc == 0) or (BestFit >= TgFit) of
		true ->
			Agent;
		false ->
			FittedAgent = fit_shc(Agent, AlgoParameters),
			#agent{scape_id = ScapeId, phenotype = FittedPhenotype, fitness = Fitness} = FittedAgent,
			FittedGenotype = ?NN_SERVICE_MODULE:phenotype_to_genotype(FittedAgent, FittedPhenotype),
			case Fitness > BestFit of
				true->
					NewParams = maps:merge(AlgoParameters, #{bestGeno => FittedGenotype, bestFit => Fitness, cycleEshc => CycleEshc - 1}),
					fit_eshc(FittedAgent, NewParams);
				false->
					{MutatedGenotype, _} = genotype_mutator:mutate(FittedGenotype, NMut, Constraint),
					phenotype:delete(FittedPhenotype),
					MutatedAgent = ?NN_SERVICE_MODULE:genotype_to_phenotype(FittedAgent, MutatedGenotype),
					?NN_SERVICE_MODULE:link_nn_to_scape(MutatedAgent, ScapeId),
					NewParams = maps:merge(AlgoParameters, #{cycleEshc => CycleEshc - 1}),
					fit_eshc(MutatedAgent#agent{fitness = 0},NewParams)
			end
	end.

fit_shc(Agent, AlgoParameters) ->
	#agent{scape_id = ScapeId, phenotype = Phenotype, fitness=CurFit} = Agent,
	#{stepnessNeuron := StepN, stepnessWeight := StepW, cycleShc := CycleShc, tgFit := TgFit} = AlgoParameters,
	case (CycleShc == 0) or (CurFit >= TgFit) of
		true ->
			Agent;
		false ->
			gen_server:call(ScapeId, reset),
			#{fitness := NewFit} = ?NN_SERVICE_MODULE:apply_to_scape(fit, Agent),
			ct:print(default, ?STD_IMPORTANCE, "FITNESS: ~p~n---------------------~n", [NewFit]),
			WeightPerturbationProbabilities = length(phenotype:get_neuron_ids(Phenotype)) * StepN / 100,
			{FittedAgent, NewParameters} = case NewFit >= CurFit of
						true ->
							?NN_SERVICE_MODULE:backup_weights(Agent),
							?NN_SERVICE_MODULE:perturb_weights(Agent, WeightPerturbationProbabilities, StepW),
							{Agent#agent{fitness = NewFit}, maps:merge(AlgoParameters, #{cycleShc => CycleShc - 1})};
						false ->
							?NN_SERVICE_MODULE:restore_weights(Agent),
							?NN_SERVICE_MODULE:backup_weights(Agent),
							?NN_SERVICE_MODULE:perturb_weights(Agent, WeightPerturbationProbabilities, StepW),
							{Agent, maps:merge(AlgoParameters, #{cycleShc => CycleShc - 1})}
				end,
			fit_shc(FittedAgent,NewParameters)
	end.
