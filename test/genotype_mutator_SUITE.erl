-module(genotype_mutator_SUITE).
-export([all/0]).
-export([clone_cortex_test/1, clone_sensor_test/1, clone_actuator_test/1, clone_neuron_classic_test/1, clone_neuron_som_test/1, clone_synapse_test/1, clone_test/1]).
-export([mutate_weights_test/1, mutate_plasticity_test/1, mutate_bias_test/1, mutate_af_test/1]).
-export([add_neuro_link_test/1, add_sensor_link_test/1, add_neuron_test/1, add_layer_neuron_test1/1, add_layer_neuron_test2/1]).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("genotype.hrl").
-include("utils.hrl").

all() -> [ 
		clone_cortex_test, clone_sensor_test, clone_actuator_test, clone_neuron_classic_test, clone_neuron_som_test, clone_synapse_test, clone_test,
		mutate_weights_test, mutate_plasticity_test, mutate_bias_test, mutate_af_test,
		add_neuro_link_test, add_sensor_link_test, add_neuron_test, add_layer_neuron_test1, add_layer_neuron_test2
	].

clone_cortex_test(_) ->
	CortexGenotype = #cortex_genotype{id = ?GETID, fit_directives = [], real_directives = []},
    {_, ClonedCortexGenotype} = genotype_mutator:clone_element(CortexGenotype),
	#cortex_genotype{fit_directives = FitDir, real_directives = RealDir} = CortexGenotype,
	#cortex_genotype{fit_directives = ClonedFitDir, real_directives = ClonedRealDir} = ClonedCortexGenotype,
    ?assert(FitDir == ClonedFitDir),
	?assert(RealDir == ClonedRealDir).

clone_sensor_test(_) ->
	SensorGenotype = #sensor_genotype{id = ?GETID, signal_input_length = 3, fit_directives = [], real_directives = []},
    {_, ClonedSensorGenotype} = genotype_mutator:clone_element(SensorGenotype),
	#sensor_genotype{signal_input_length = SignalLen, fit_directives = FitDir, real_directives = RealDir} = SensorGenotype,
	#sensor_genotype{signal_input_length = ClonedSignalLen, fit_directives = ClonedFitDir, real_directives = ClonedRealDir} = ClonedSensorGenotype,
	?assert(SignalLen == ClonedSignalLen),
    ?assert(FitDir == ClonedFitDir),
	?assert(RealDir == ClonedRealDir).

clone_actuator_test(_) ->
    ActuatorGenotype = #actuator_genotype{id = ?GETID, number_of_clients = 4, fit_directives = [], real_directives = []},
    {_, ClonedActuatorGenotype} = genotype_mutator:clone_element(ActuatorGenotype),
	#actuator_genotype{number_of_clients = Clients, fit_directives = FitDir, real_directives = RealDir} = ActuatorGenotype,
	#actuator_genotype{number_of_clients = ClonedClients, fit_directives = ClonedFitDir, real_directives = ClonedRealDir} = ClonedActuatorGenotype,
	?assert(Clients == ClonedClients),
    ?assert(FitDir == ClonedFitDir),
	?assert(RealDir == ClonedRealDir).

clone_neuron_classic_test(_) ->
	NeuronGenotype = #neuron_classic_genotype{id = ?GETID, layer = 2, bias = 0.75, activation_function = sigmund},
    {_, ClonedNeuronGenotype} = genotype_mutator:clone_element(NeuronGenotype),
	#neuron_classic_genotype{layer = Layer, bias = Bias, activation_function = Af} = NeuronGenotype,
	#neuron_classic_genotype{layer = ClonedLayer, bias = ClonedBias, activation_function = ClonedAf} = ClonedNeuronGenotype,
    ?assert(Layer == ClonedLayer),
    ?assert(Bias == ClonedBias),
	?assert(Af == ClonedAf).

clone_neuron_som_test(_) ->
	NeuronGenotype = #neuron_som_genotype{id = ?GETID, coordinates = {3, 5}, weight = [1, 3, 4], cluster = 3, activation_function = euclidean},
    {_, ClonedNeuronGenotype} = genotype_mutator:clone_element(NeuronGenotype),
	#neuron_som_genotype{coordinates = Coord, weight = Weight, cluster = Cluster, activation_function = Af} = NeuronGenotype,
	#neuron_som_genotype{coordinates = ClonedCoord, weight = ClonedWeight, cluster = ClonedCluster, activation_function = ClonedAf} = ClonedNeuronGenotype,
	?assert(Coord == ClonedCoord),
    ?assert(Weight == ClonedWeight),
    ?assert(Cluster == ClonedCluster),
	?assert(Af == ClonedAf).

clone_synapse_test(_) ->
	SynapseGenotype = #synapses{id_from = 3, id_to = 4, weight = [3], plasticity_modulation = none, tag = {neuron, neuron}, connection_direction = recurrent},
    MappingIds = [{3, 5}, {4, 6}],
	{_, ClonedSynapse} = genotype_mutator:clone_synapse(SynapseGenotype, MappingIds),
	#synapses{id_from = IdFrom, id_to = IdTo, weight = Weight, plasticity_modulation = Plast, tag = Tag, connection_direction = Dir} = SynapseGenotype,
	#synapses{id_from = ClonedIdFrom, id_to = ClonedIdTo, weight = ClonedWeight, plasticity_modulation = ClonedPlast, tag = ClonedTag, connection_direction = ClonedDir} = ClonedSynapse,
	%%Endpoints of the cloned synapses must switched
	?assert(ClonedIdFrom == 5),
    ?assert(ClonedIdTo == 6),
    ?assert(Weight == ClonedWeight),
	?assert(Plast == ClonedPlast),
    ?assert(Tag == ClonedTag),
	?assert(Dir == ClonedDir).

clone_test(_) ->
	Genotype = get_default_genotype_with_layers(1),
	ClonedGenotype = genotype_mutator:clone(Genotype),
	CortexGenotype = genotype:get_cortex(ClonedGenotype),
	Sensors = genotype:get_sensors(ClonedGenotype),
	Actuators = genotype:get_actuators(ClonedGenotype),
	Neurons = genotype:get_neurons(ClonedGenotype),
	%Check if there is the same number of elements of the original genotype. The value of cloned elements are insured from the previous tests
	?assert(length(Sensors) == 1),
	?assert(length(Actuators) == 1),
	?assert(length(Neurons) == 1).


mutate_weights_test(_) ->
	Genotype = get_default_genotype_with_layers(1),
	SynapsesGenotype = [genotype:get_synapses(Genotype, SynapseIdFrom, SynapseIdTo) || {SynapseIdFrom, SynapseIdTo} <- genotype:get_synapses_ids(Genotype)],
	{NewGenotype, Mutation} = genotype_mutator:mutate_weights(Genotype, null),
	#{type := weights, synapse_id := {IdFrom, IdTo}, old_weights := OldWeight, new_weights := NewWeight} = Mutation,
	%Retrieve mutated synapse and check the mutation
	MutatedSynapse = genotype:get_synapses(NewGenotype, IdFrom, IdTo),
	Pred = fun(SynapseGenotype) -> SynapseGenotype#synapses.id_from == IdFrom andalso SynapseGenotype#synapses.id_to == IdTo end ,
	[OldSynapse] = lists:filter(Pred, SynapsesGenotype),
	?assert(OldSynapse#synapses.id_from == MutatedSynapse#synapses.id_from),
	?assert(OldSynapse#synapses.id_to == MutatedSynapse#synapses.id_to),
	?assert(MutatedSynapse#synapses.weight == NewWeight),
	?assert(OldSynapse#synapses.weight /= MutatedSynapse#synapses.weight),
	?assert(OldSynapse#synapses.plasticity_modulation == MutatedSynapse#synapses.plasticity_modulation),
	?assert(OldSynapse#synapses.tag == MutatedSynapse#synapses.tag),
	?assert(OldSynapse#synapses.connection_direction == MutatedSynapse#synapses.connection_direction).

mutate_plasticity_test(_) ->
	Genotype = get_default_genotype_with_layers(1),
	SynapsesGenotype = [genotype:get_synapses(Genotype, SynapseIdFrom, SynapseIdTo) || {SynapseIdFrom, SynapseIdTo} <- genotype:get_synapses_ids(Genotype)],
	{NewGenotype, Mutation} = genotype_mutator:mutate_plasticity(Genotype, [{plast, plasticity:all() -- [none]}]),
	#{type := plasticity, synapse_id := {IdFrom, IdTo}, old_plasticity := OldPlasticity, new_plasticity := NewPlasticity} = Mutation,
	%Retrieve mutated synapse and check the mutation
	MutatedSynapse = genotype:get_synapses(NewGenotype, IdFrom, IdTo),
	Pred = fun(SynapseGenotype) -> SynapseGenotype#synapses.id_from == IdFrom andalso SynapseGenotype#synapses.id_to == IdTo end ,
	[OldSynapse] = lists:filter(Pred, SynapsesGenotype),
	?assert(OldSynapse#synapses.id_from == MutatedSynapse#synapses.id_from),
	?assert(OldSynapse#synapses.id_to == MutatedSynapse#synapses.id_to),
	?assert(OldSynapse#synapses.weight == MutatedSynapse#synapses.weight),
	?assert(OldSynapse#synapses.plasticity_modulation /= MutatedSynapse#synapses.plasticity_modulation),
	?assert(OldSynapse#synapses.tag == MutatedSynapse#synapses.tag),
	?assert(OldSynapse#synapses.connection_direction == MutatedSynapse#synapses.connection_direction).

mutate_bias_test(_) ->
	Genotype = get_default_genotype_with_layers(1),
	[OldNeuron] = genotype:get_neurons(Genotype),
	{NewGenotype, Mutation} = genotype_mutator:mutate_bias(Genotype, null),
	#{type := bias, neuron_id := NeuronId, old_bias := OldBias, new_bias := NewBias} = Mutation,
	%Retrieve mutated neuron and check the mutation
	[MutatedNeuron] = genotype:get_neurons(NewGenotype),
	?assert(OldNeuron#neuron_classic_genotype.id == MutatedNeuron#neuron_classic_genotype.id),
	?assert(OldNeuron#neuron_classic_genotype.layer == MutatedNeuron#neuron_classic_genotype.layer),
	?assert(OldNeuron#neuron_classic_genotype.bias /= MutatedNeuron#neuron_classic_genotype.bias),
	?assert(OldNeuron#neuron_classic_genotype.activation_function == MutatedNeuron#neuron_classic_genotype.activation_function).

mutate_af_test(_) ->
	Genotype = get_default_genotype_with_layers(1),
	[OldNeuron] = genotype:get_neurons(Genotype),
	{NewGenotype, Mutation} = genotype_mutator:mutate_af(Genotype, [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic() -- [rectifier]}]),
	#{type := activation_function, neuron_id := NeuronId, old_af := OldAf, new_af := NewAf} = Mutation,
	%Retrieve mutated neuron and check the mutation
	[MutatedNeuron] = genotype:get_neurons(NewGenotype),
	?assert(OldNeuron#neuron_classic_genotype.id == MutatedNeuron#neuron_classic_genotype.id),
	?assert(OldNeuron#neuron_classic_genotype.layer == MutatedNeuron#neuron_classic_genotype.layer),
	?assert(OldNeuron#neuron_classic_genotype.bias == MutatedNeuron#neuron_classic_genotype.bias),
	?assert(OldNeuron#neuron_classic_genotype.activation_function /= MutatedNeuron#neuron_classic_genotype.activation_function),
	?assert(MutatedNeuron#neuron_classic_genotype.activation_function == NewAf).

add_neuro_link_test(_) ->
	Genotype = get_genotype_no_neuron_links(),
	{NewGenotype, Mutation} = genotype_mutator:add_neuro_link(Genotype, [{plast, plasticity:all()}]),
	#{type := neuro_link, link_from := NeuronIdFrom, link_to := NeuronIdTo} = Mutation,
	?assert(genotype:get_synapses(NewGenotype, NeuronIdFrom, NeuronIdTo) /= false).

add_sensor_link_test(_) ->
	Genotype = get_genotype_no_sensor_links(),
	{NewGenotype, Mutation} = genotype_mutator:add_sensor_link(Genotype, [{plast, plasticity:all()}]),
	#{type := sensor_link, link_from := SensorId, link_to := NeuronId} = Mutation,
	?assert(genotype:get_synapses(NewGenotype, SensorId, NeuronId) /= false).

add_neuron_test(_) ->
	Genotype = get_default_genotype_with_layers(2),
	{NewGenotype, Mutation} = genotype_mutator:add_neuron(Genotype, [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic()}, {plast, plasticity:all()}]),
	#{type := add_neuron, neuron_id := NewNeuronId, connected_with := {IdIncoming, IdOutgoing}} = Mutation, 
	?assert(genotype:get_element_by_id(NewGenotype, NewNeuronId) /= false),
	?assert(genotype:get_synapses(NewGenotype, IdIncoming, NewNeuronId) /= false),
	?assert(genotype:get_synapses(NewGenotype, NewNeuronId, IdOutgoing) /= false).

add_layer_neuron_test1(_) ->
	Genotype = get_default_genotype_with_layers(1),
	{NewGenotype, Mutation} = genotype_mutator:add_layer_neuron(Genotype, [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic()}, {plast, plasticity:all()}]),
	#{type := add_layer_neuron, neuron_id := NewNeuronId, connected_with := {IdIncoming, IdOutgoing}} = Mutation, 
	?assert(genotype:get_element_by_id(NewGenotype, NewNeuronId) /= false),
	?assert(genotype:get_synapses(NewGenotype, IdIncoming, NewNeuronId) /= false),
	?assert(genotype:get_synapses(NewGenotype, NewNeuronId, IdOutgoing) /= false).

add_layer_neuron_test2(_) ->
	Genotype = get_default_genotype_with_layers(2),
	{NewGenotype, Mutation} = genotype_mutator:add_layer_neuron(Genotype, [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic()}, {plast, plasticity:all()}]),
	#{type := add_layer_neuron, neuron_id := NewNeuronId, connected_with := {IdIncoming, IdOutgoing}} = Mutation, 
	?assert(genotype:get_element_by_id(NewGenotype, NewNeuronId) /= false),
	?assert(genotype:get_synapses(NewGenotype, IdIncoming, NewNeuronId) /= false),
	?assert(genotype:get_synapses(NewGenotype, NewNeuronId, IdOutgoing) /= false).

%%private functions
 get_default_genotype_with_layers(NumLayers) ->
	NeuronsSpec = {ffnn, rectifier, none},
	SensorSpec = {2, [], []},
	ActuatorSpec = {1, [], []},
	CortexSpec = {[], []},
	LayerDensity = lists:duplicate(NumLayers - 1, 1),
	Genotype = genotype:create_NN(NeuronsSpec, SensorSpec, ActuatorSpec, CortexSpec, LayerDensity),
	Genotype.

get_genotype_no_neuron_links() ->
	Genotype = genotype:new(classic),
	genotype:add_element_with_genotype(Genotype, #sensor_genotype{id = 1, signal_input_length = 3}),
	genotype:add_element_with_genotype(Genotype, #neuron_classic_genotype{id = 2, layer = 1, bias = 4}),
	genotype:add_element_with_genotype(Genotype, #neuron_classic_genotype{id = 3, layer = 2, bias = 5}),
	genotype:add_element_with_genotype(Genotype, #actuator_genotype{id = 4, number_of_clients = 1}),
	genotype:add_element_with_genotype(Genotype, #cortex_genotype{id = 5}),
	genotype:add_element_with_genotype(Genotype, #synapses{id_from = 1, id_to = 2}),
	genotype:add_element_with_genotype(Genotype, #synapses{id_from = 3, id_to = 4}),
	Genotype.

get_genotype_no_sensor_links() ->
	Genotype = genotype:new(classic),
	genotype:add_element_with_genotype(Genotype, #sensor_genotype{id = 1, signal_input_length = 3}),
	genotype:add_element_with_genotype(Genotype, #neuron_classic_genotype{id = 2, layer = 1, bias = 4}),
	genotype:add_element_with_genotype(Genotype, #neuron_classic_genotype{id = 3, layer = 2, bias = 5}),
	genotype:add_element_with_genotype(Genotype, #actuator_genotype{id = 4, number_of_clients = 1}),
	genotype:add_element_with_genotype(Genotype, #cortex_genotype{id = 5}),
	genotype:add_element_with_genotype(Genotype, #synapses{id_from = 3, id_to = 4}),
	Genotype.