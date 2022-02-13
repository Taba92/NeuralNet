-module(neuron_som).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{lastOutput, lastSignals, phenotype}).
-include("utils.hrl").
-include("phenotype.hrl").

init(Phenotype) when is_record(Phenotype, neuron_som_phenotype) ->
	#neuron_som_phenotype{id = Id} = Phenotype,
	gen_server:start_link({local, Id}, ?MODULE, [Phenotype], []);
init([Phenotype]) ->
	State = #state{phenotype = Phenotype},
	{ok, State}.

handle_call(get, _, State) ->
	#state{phenotype = Phenotype} = State,
	{reply, Phenotype, State};
handle_call({update, {update_weight, LearnRate, {Mod, NeighboorFun, PartialArgs}}}, _, State) ->
	#state{lastSignals = Signal, phenotype = Phenotype} = State,
	#neuron_som_phenotype{coordinates = Coord, weight = Weight} = Phenotype,
	Scalar = LearnRate * erlang:apply(Mod, NeighboorFun, [Coord | PartialArgs]),
	SubVect = [X - Y || {X, Y} <- lists:zip(Signal, Weight)],
	DeltaVect = [X * Y || X<- [Scalar], Y <- SubVect],
	NewWeight = [X + Y || {X, Y} <- lists:zip(Weight, DeltaVect)],
	NewPhenotype = Phenotype#neuron_som_phenotype{weight = NewWeight},
	NewState = State#state{phenotype = NewPhenotype},
	{reply, ok, NewState};
handle_call({update, {cluster_setting, Centroids}}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#neuron_som_phenotype{activation_function = ActivationFun, weight = Weight} = Phenotype,
	Dists = [math_utils:ActivationFun(Weight, Centroid) || Centroid <- Centroids],
	MinDist = lists:min(Dists),
	ClusterMap = lists:zip(lists:seq(1, length(Centroids)), Dists),
	{NumCluster, _} = lists:keyfind(MinDist, 2, ClusterMap),
	NewPhenotype = Phenotype#neuron_som_phenotype{cluster = NumCluster},
	{reply, ok, State#state{phenotype = NewPhenotype}};
handle_call({add_synapses, {IdFrom, IdTo, Tag, _Weight, _Modulation, ConnectionDirection}}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#neuron_som_phenotype{id = Id, input_elements_data = InputSynapses, neighbors_data = NeightborSynapses, output_elements_ids = OutputSynapses} = Phenotype,
	%Check if the node will be the sender or the receiver and if is input/output synapse or a neightbour synapse
	NewPhenotype = case {Id, Tag} of
						%Sender and neighbor synapse
						{IdFrom, {neuron, neuron}} -> 
							Phenotype#neuron_som_phenotype{neighbors_data = NeightborSynapses ++ [{IdTo, ConnectionDirection}]};
						%Sender and output synapse
						{IdFrom, {neuron, actuator}} ->
							Phenotype#neuron_som_phenotype{output_elements_ids = OutputSynapses ++ [IdTo]};
						% Receiver and neighbor synapse
						{IdTo, {neuron, neuron}} ->
							Phenotype#neuron_som_phenotype{neighbors_data = NeightborSynapses ++ [{IdFrom, ConnectionDirection}]};
						% Receiver and input synapse
						{IdTo, {NodeTypeFrom, neuron}} ->
							Phenotype#neuron_som_phenotype{input_elements_data = InputSynapses ++ [{IdFrom, NodeTypeFrom}]}
					end,
	{reply, ok, State#state{phenotype = NewPhenotype}};
handle_call({delete_synapses, IdFrom, IdTo, Tag}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#neuron_som_phenotype{id = Id, input_elements_data = InputSynapses, neighbors_data = NeightborSynapses, output_elements_ids = OutputSynapses} = Phenotype,
	%Check if the node will be the sender or the receiver and if is input/output synapse or a neightbour synapse
	NewPhenotype = case {Id, Tag} of
						%Sender and neighbor synapse
						{IdFrom, {neuron, neuron}} ->
							%Get the neighbor synapse related to IdTo
							NeighborSynapse = lists:keyfind(IdTo, 1, NeightborSynapses), 
							Phenotype#neuron_som_phenotype{neighbors_data = NeightborSynapses -- [NeighborSynapse]};
						%Sender and output synapse
						{IdFrom, {neuron, actuator}} ->
							Phenotype#neuron_som_phenotype{output_elements_ids = OutputSynapses -- [IdTo]};
						% Receiver and neighbor synapse
						{IdTo, {neuron, neuron}} ->
							%Get the neighbor synapse related to IdFrom
							NeighborSynapse = lists:keyfind(IdFrom, 1, NeightborSynapses),
							Phenotype#neuron_som_phenotype{neighbors_data = NeightborSynapses -- [NeighborSynapse]};
						% Receiver and input synapse
						{IdTo, {NodeTypeFrom, neuron}} ->
							Phenotype#neuron_classic_phenotype{input_elements_data = InputSynapses -- [{IdFrom, NodeTypeFrom}]}
					end,
	{reply, ok, State#state{phenotype = NewPhenotype}}.

terminate(normal, _) -> ok.

handle_cast({ElType, _, _, forward_fit, Signal}, State) when ElType == sensor ->
	#state{phenotype = Phenotype} = State,
	#neuron_som_phenotype{id = NeuronId, coordinates = Coord, activation_function = ActivationFun, weight = Weight, output_elements_ids = OutputSynapses} = Phenotype,
	Dist = math_utils:ActivationFun(Weight, Signal),
	[gen_server:cast(NeuronId, {neuron, Coord, Id, forward_fit, [Dist]}) || Id <- OutputSynapses],
	NewState =State#state{lastOutput = Dist, lastSignals = Signal},
	{noreply, NewState};
handle_cast({ElType, _, _, FwdType, Signal}, State) when ElType == sensor ->
	#state{phenotype = Phenotype} = State,
	#neuron_som_phenotype{id = NeuronId, coordinates = Coord, activation_function = ActivationFun, weight = Weight, cluster = Cluster, output_elements_ids = OutputSynapses} = Phenotype,
	Dist = math_utils:ActivationFun(Weight, Signal),
	[gen_server:cast(NeuronId,{neuron, {Coord, Cluster}, Id, FwdType, [Dist]}) || Id <- OutputSynapses],
	NewState = State#state{lastOutput = Dist, lastSignals = Signal},
	{noreply, NewState}.