-module(genotype_mutator).
-export([mutate/3,mutate_weights/2,mutate_plasticity/2,mutate_bias/2,mutate_af/2,add_neuro_link/2,add_sensor_link/2,
		add_layer_neuron/2, add_neuron/2, clone/1]).
-include("utils.hrl").
-include("genotype.hrl").
-include("phenotype.hrl").

%%SUPPORTED ONLY MUTATIONS OF CLASSIC NEURONS
%%IN FUTURE WILL BE SUPPORTED ALSO SOM NEURONS

clone(Genotype) ->
	#genotype{network_type = Networktype, network = Network} = Genotype,
	%1) Initialize a new empty genotype
	NewGenotype = genotype:new(Networktype),
	%2) Clone elements in the new genotype and create mapping between old ids and new ids
	CloneNodeFun = fun(ElementId, Acc) ->
						ElementGenotype = genotype:get_element_by_id(Genotype, ElementId),
						{NewElementId, NewElementGenotype} = clone_element(ElementGenotype),
						genotype:add_element_with_genotype(NewGenotype, NewElementGenotype),
						[{ElementId, NewElementId} | Acc]
				   end,
	MappingIds = lists:foldl(CloneNodeFun, [], genotype:get_elements_ids(Genotype)),
	%3) Clone synapses in the new genotype
	CloneSynapseFun = fun({IdFrom, IdTo}) ->
						SynapseGenotype = genotype:get_synapses(Genotype, IdFrom, IdTo),
						{_, NewSynapseGenotype} = clone_synapse(SynapseGenotype, MappingIds),
						genotype:add_element_with_genotype(NewGenotype, NewSynapseGenotype)
					  end,
	lists:map(CloneSynapseFun, genotype:get_synapses_ids(Genotype)),
	NewGenotype.

clone_element(ElementGenotype) when is_record(ElementGenotype, cortex_genotype) ->
	NewId = ?GETID,
	{NewId, ElementGenotype#cortex_genotype{id = NewId}};
clone_element(ElementGenotype) when is_record(ElementGenotype, sensor_genotype) ->
	NewId = ?GETID,
	{NewId, ElementGenotype#sensor_genotype{id = NewId}};
clone_element(ElementGenotype) when is_record(ElementGenotype, actuator_genotype) ->
	NewId = ?GETID,
	{NewId, ElementGenotype#actuator_genotype{id = NewId}};
clone_element(ElementGenotype) when is_record(ElementGenotype, neuron_classic_genotype) ->
	NewId = ?GETID,
	{NewId, ElementGenotype#neuron_classic_genotype{id = NewId}};
clone_element(ElementGenotype) when is_record(ElementGenotype, neuron_som_genotype) ->
	NewId = ?GETID,
	{NewId, ElementGenotype#neuron_som_genotype{id = NewId}}.

clone_synapse(SynapseGenotype, MappingIds) when is_record(SynapseGenotype, synapses) ->
	#synapses{id_from = IdFrom, id_to = IdTo} = SynapseGenotype,
	%1) Extract new nodes ids of the edge
	{IdFrom, NewIdFrom} = lists:keyfind(IdFrom, 1, MappingIds),
	{IdTo, NewIdTo} = lists:keyfind(IdTo, 1, MappingIds),
	SynapseGenotype#synapses{id_from = NewIdFrom, id_to = NewIdTo}.

mutate(Genotype, NMutation, Constraint)->
	{MutatorsConstraint, SpecificsConstraint} = parse_constraint(Constraint),
	Mutators = [?RANDCHOOSE(MutatorsConstraint) || _ <- lists:seq(1, NMutation)],
	lists:foldl(fun(Fun, Geno) -> ?MODULE:Fun(Geno, SpecificsConstraint) end, {Genotype, []}, Mutators).


parse_constraint(none) ->
	{get_mutators(), [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic()}, {plast, plasticity:all()}]};
parse_constraint(#{mutators := none, af := none, plast := none}) -> 
	parse_constraint(none);
parse_constraint(#{mutators := none, af := Afs , plast := Plasts}) when is_list(Afs),is_list(Plasts) ->
	{get_mutators(), [{af, Afs}, {plast, Plasts}]};
parse_constraint(#{mutators := none, af := Afs, plast := none}) when is_list(Afs) ->
	{get_mutators(), [{af, Afs}, {plast, plasticity:all()}]};
parse_constraint(#{mutators := none, af := none, plast := Plasts}) when is_list(Plasts) ->
	{get_mutators(), [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic()}, {plast, Plasts}]};
parse_constraint(#{mutators := Mutators, af := none, plast := Plasts}) when is_list(Mutators),is_list(Plasts) ->
	{Mutators, [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic()}, {plast, Plasts}]};
parse_constraint(#{mutators := Mutators, af := Afs, plast := none}) when is_list(Mutators),is_list(Afs) ->
	{Mutators, [{af, Afs}, {plast, plasticity:all()}]};
parse_constraint(#{mutators := Mutators, af := none, plast := none}) when is_list(Mutators) ->
	{Mutators, [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic()}, {plast, plasticity:all()}]};
parse_constraint(#{mutators := Mutators, af := Afs, plast := Plasts}) when is_list(Mutators),is_list(Afs),is_list(Plasts) ->
	{Mutators, [{af, Afs}, {plast, Plasts}]}.

get_mutators() ->
	[mutate_weights, mutate_bias, mutate_af, add_neuro_link, add_sensor_link, add_layer_neuron, add_neuron, mutate_plasticity].

mutate_weights({Genotype, Mutations}, _) ->
	%1) Select a random edge id
	{IdFrom, IdTo} = ?RANDCHOOSE(genotype:get_synapses_ids(Genotype)),
	SynapseGenotype = genotype:get_synapses(Genotype, IdFrom, IdTo),
	%2) Perturb the weight
	NewWeight = [?NN_SERVICE_MODULE:perturbate(X) || X <- SynapseGenotype#synapses.weight],
	NewSynapseGenotype = SynapseGenotype#synapses{weight = NewWeight},
	%3) Update the synapse
	genotype:update_synapse_genotype(Genotype, IdFrom, IdTo, NewSynapseGenotype),
	Mutation = #{type => weights, synapse_id => {IdFrom, IdTo}, old_weights => SynapseGenotype#synapses.weight, new_weights => NewWeight},
	{Genotype, Mutations ++ [Mutation]}.

mutate_plasticity({Genotype, Mutations}, Constraint) ->
	{plast, Plasts} = lists:keyfind(plast, 1, Constraint),
	%1) Select a random edge id
	{IdFrom, IdTo} = ?RANDCHOOSE(genotype:get_synapses_ids(Genotype)),
	SynapseGenotype = genotype:get_synapses(Genotype, IdFrom, IdTo),
	%2) Modify the plasticity of the synapse
	NewPlasticity = plasticity:get_rand_plast(SynapseGenotype#synapses.weight, Plasts),
	NewSynapseGenotype = SynapseGenotype#synapses{plasticity_modulation = NewPlasticity},
	%3) Update the synapse 
	genotype:update_synapse_genotype(Genotype, IdFrom, IdTo, NewSynapseGenotype),
	Mutation = #{type => plasticity, synapse_id => {IdFrom, IdTo}, old_plasticity => SynapseGenotype#synapses.plasticity_modulation, new_plasticity => NewPlasticity},
	{Genotype, Mutations ++ [Mutation]}.

mutate_bias({Genotype, Mutations}, _) ->
	%1) Select random neuron
	NeuronId = ?RANDCHOOSE(genotype:get_neuron_ids(Genotype)),
	NeuronGenotype = genotype:get_element_by_id(NeuronId),
	%2) Perturb the bias
	NewBias = [?NN_SERVICE_MODULE:perturbate(X) || X <- NeuronGenotype#neuron_classic_genotype.bias],
	NewNeuronGenotype = NeuronGenotype#neuron_classic_genotype{bias = NewBias},
	%3) Update the neuron
	genotype:update_element_genotype(Genotype, NeuronId, NewNeuronGenotype),
	Mutation = #{type => bias, neuron_id => NeuronId, old_bias => NeuronGenotype#neuron_classic_genotype.bias, new_bias => NewBias},
	{Genotype, Mutations ++ [Mutation]}.

mutate_af({Genotype, Mutations}, Constraint)->
	{af,Afs} = lists:keyfind(af, 1, Constraint),
	%1) Select random neuron
	NeuronId = ?RANDCHOOSE(genotype:get_neuron_ids(Genotype)),
	NeuronGenotype = genotype:get_element_by_id(NeuronId),
	%2) Modify the activation function
	NewAf = ?RANDCHOOSE(Afs),
	NewNeuronGenotype = NeuronGenotype#neuron_classic_genotype{activation_function = NewAf},
	%3) Update the neuron
	genotype:update_element_genotype(Genotype, NeuronId, NewNeuronGenotype),
	Mutation = #{type => activation_function, neuron_id => NeuronId, old_af => NeuronGenotype#neuron_classic_genotype.activation_function, new_af => NewAf},
	{Genotype, Mutations ++ [Mutation]}.

add_neuro_link({Genotype, Mutations}, Constraint) ->%link tra neuroni,o su se stesso oppure con un altro neurone in qualsiasi strato.
	{plast, Plasts} = lists:keyfind(plast, 1, Constraint),
	%1) Select two random neurons
	{NeuronIdFrom, NeuronIdTo} = {?RANDCHOOSE(genotype:get_neuron_ids(Genotype)), ?RANDCHOOSE(genotype:get_neuron_ids(Genotype))},
	{NeuronFromGenotype, NeuronToGenotype} = {genotype:get_element_by_id(NeuronIdFrom), genotype:get_element_by_id(NeuronIdTo)},
	%2) Add the synapse between the two neurons if not already exist
	case genotype:get_synapses(Genotype, NeuronIdFrom, NeuronIdTo) of%se non c'è un link allora muto se no non muto
		false ->
			%2.1) Get the synapse direction
			ConnectionDirection = case NeuronToGenotype#neuron_classic_genotype.layer =< NeuronFromGenotype#neuron_classic_genotype.layer of
									true -> recurrent;
									false -> forward
								  end,
			%2.2) Create the plasticity for the synapse
			PlasticityModulator = plasticity:random_plasticity(Plasts),
			%2.3) Add the synapse
			genotype:add_synapses(Genotype, NeuronIdFrom, NeuronIdTo, #{signal_len => 1, tag => {neuron, neuron}, modulation_type => PlasticityModulator, connection_direction => ConnectionDirection}),
			Mutation = #{type => neuro_link, link_from => NeuronIdFrom, link_to => NeuronIdTo},
			{Genotype, Mutations ++ [Mutation]};
		_ -> 
			{Genotype, Mutations}
	end.

add_sensor_link({Genotype, Mutations}, Constraint) ->
	{plast,Plasts} = lists:keyfind(plast, 1, Constraint),
	%1) Select a random sensor and a random neuron
	{SensorId, NeuronId} = {?RANDCHOOSE(genotype:get_neurons_ids(Genotype)), ?RANDCHOOSE(genotype:get_sensors_ids(Genotype))},
	{SensorGenotype, NeuronGenotype} = {genotype:get_element_by_id(SensorId), genotype:get_element_by_id(NeuronId)},
	%2) Add the synapse between the sensor and the neuron if not already exist
	case genotype:get_synapses(Genotype, SensorId, NeuronId) of
		false->
			%2.1) Create the plasticity for the synapse
			PlasticityModulator = plasticity:random_plasticity(Plasts),
			%2.2) Add the synapse
			genotype:add_synapses(Genotype, SensorId, NeuronId, #{signal_len => SensorGenotype#sensor_genotype.signal_input_length, tag => {sensor, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}),
			Mutation = #{type => sensor_link, link_from => SensorId, link_to => NeuronId},
			{Genotype, Mutations ++ [Mutation]};
		true ->
			{Genotype, Mutations}
	end.

add_neuron({Genotype, Mutations}, Constraint)->
	{plast, Plasts} = lists:keyfind(plast, 1, Constraint),
	{af, Afs} = lists:keyfind(af, 1, Constraint),
	Layers = genotype:get_layers(Genotype),
	[H | T] = Layers,
	case length(Layers) of
		1 -> 
			%If the only one layer no mutation, because it would be connected to the actuator and consequently the length of the output signal would be increased. 
			%But the scape only accepts signals of a certain length 
			{Genotype, Mutations};
		_ -> 
			LayerWhereInsertTheNeuron = ?RANDCHOOSE(lists:droplast(Layers)),
			PlasticityModulator = plasticity:random_plasticity(Plasts),
			ActivationFunction = ?RANDCHOOSE(Afs),
			%1) Create the new neuron
			NewNeuronId = genotype:add_neuron(Genotype, #{layer => LayerWhereInsertTheNeuron, activation_function => ActivationFunction}),
			case LayerWhereInsertTheNeuron of
				%A neuron will be inserted at the first layer
				H -> 
					%1) Select a random sensor
					SensorId = ?RANDCHOOSE(genotype:get_sensors_ids(Genotype)),
					%2) Select a random neuron of a layer greater than the current layer
					Predicate = fun(Element) -> is_record(Element, neuron_classic_genotype) andalso Element#neuron_classic_genotype.layer > LayerWhereInsertTheNeuron end,
					SelectFun = fun(Neuron) -> Neuron#neuron_classic_genotype.id end,
					NeuronId = genotype:get_select_on_elements_filtered(Genotype, Predicate, SelectFun),
					%3) Get the genotype of the sensor
					SensorGenotype = genotype:get_element_by_id(SensorId),
					%4) Add the synapses between sensor -> new neuron and new neuron -> NeuronId
					genotype:add_synapses(Genotype, SensorId, NewNeuronId, #{signal_len => SensorGenotype#sensor_genotype.signal_input_length, tag => {sensor, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}),
					genotype:add_synapses(Genotype, NewNeuronId, NeuronId, #{signal_len => 1, tag => {neuron, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}),
					Mutation = #{type => add_neuron, neuron_id => NewNeuronId, connected_with => {SensorId, NeuronId}},
					{Genotype, Mutations ++ [Mutation]};
				%A neuron will be inserted in some hidden layer
				_ ->
					SelectFun = fun(Neuron) -> Neuron#neuron_classic_genotype.id end,
					%1) Get a random neuron from a layer more less than current layer
					PredicateInferior = fun(Element) -> is_record(Element, neuron_classic_genotype) andalso Element#neuron_classic_genotype.layer < LayerWhereInsertTheNeuron end,
					NeuronInferiorId = genotype:get_select_on_elements_filtered(Genotype, PredicateInferior, SelectFun),
					%2) Get a random neuron from a layer greater than current layer
					PredicateSuperior = fun(Element) -> is_record(Element, neuron_classic_genotype) andalso Element#neuron_classic_genotype.layer > LayerWhereInsertTheNeuron end,
					NeuronSuperiorId = genotype:get_select_on_elements_filtered(Genotype, PredicateSuperior, SelectFun),
					%3) Add the synapses between neuron inf -> new neuron and new neuron -> neuron sup
					genotype:add_synapses(Genotype, NeuronInferiorId, NewNeuronId, #{signal_len => 1, tag => {neuron, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}),
					genotype:add_synapses(Genotype, NewNeuronId, NeuronSuperiorId, #{signal_len => 1, tag => {neuron, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}),
					Mutation = #{type => add_neuron, neuron_id => NewNeuronId, connected_with => {NeuronInferiorId, NeuronSuperiorId}},
					{Genotype, Mutations ++ [Mutation]}
			end
	end.

add_layer_neuron({Genotype, Mutations}, Constraint) ->%add a new layer between two other layers
	{plast, Plasts} = lists:keyfind(plast, 1, Constraint),
	{af, Afs} = lists:keyfind(af, 1, Constraint),
	PlasticityModulator = plasticity:random_plasticity(Plasts),
	ActivationFunction = ?RANDCHOOSE(Afs),
	Layers = genotype:get_layers(Genotype),
	[FirstLayer | _] = Layers,
	case length(Layers) of
		%If i have only one layer, create a connection between a random sensor -> new neuron and new neuron -> random neuron of first layer
		1 -> 
			%1) Create the new neuron
			NewNeuronId = genotype:add_neuron(Genotype, #{layer => FirstLayer / 2, activation_function => ActivationFunction}),
			%2) Select a random sensor
			SensorId = ?RANDCHOOSE(genotype:get_sensors_ids(Genotype)),
			%3) Select a random neuron of the first layer
			Predicate = fun(Element) -> is_record(Element, neuron_classic_genotype) andalso Element#neuron_classic_genotype.layer == FirstLayer end,
			SelectFun = fun(Neuron) -> Neuron#neuron_classic_genotype.id end,
			NeuronId = ?RANDCHOOSE(genotype:get_select_on_elements_filtered(Genotype, Predicate, SelectFun)),
			%4) Create the connection from the sensor to the new neuron
			SensorGenotype = genotype:get_element_by_id(SensorId),
			genotype:add_synapses(Genotype, SensorId, NewNeuronId, #{signal_len => SensorGenotype#sensor_genotype.signal_input_length, tag => {sensor, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}), 
			%5) Create the connection from the new neuron to the random neuron
			genotype:add_synapses(Genotype, NewNeuronId, NeuronId, #{signal_len => 1, tag => {neuron, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}),
			Mutation = #{type => add_layer_neuron, neuron_id => NewNeuronId, connected_with => {SensorId, NeuronId}},
			{Genotype, Mutations ++ [Mutation]};
		N when N > 1 ->
			LayerReference = ?RANDCHOOSE(lists:droplast(Layers)),
			%1) Choose randomly if insert the new layer before or after the reference layer above
			case ?RANDCHOOSE([0,1]) of
				% Insert before the reference layer
		 		0 ->
					case LayerReference of
						% If the layer reference is the first layer, equal to the case with only 1 layer
						FirstLayer -> 
							%1) Create the new neuron
							NewNeuronId = genotype:add_neuron(Genotype, #{layer => FirstLayer / 2, activation_function => ActivationFunction}),
							%2) Select a random sensor
							SensorId = ?RANDCHOOSE(genotype:get_sensors_ids(Genotype)),
							%3) Select a random neuron of the first layer
							Predicate = fun(Element) -> is_record(Element, neuron_classic_genotype) andalso Element#neuron_classic_genotype.layer == FirstLayer end,
							SelectFun = fun(Neuron) -> Neuron#neuron_classic_genotype.id end,
							NeuronId = ?RANDCHOOSE(genotype:get_select_on_elements_filtered(Genotype, Predicate, SelectFun)),
							%4) Create the connection from the sensor to the new neuron
							SensorGenotype = genotype:get_element_by_id(SensorId),
							genotype:add_synapses(Genotype, SensorId, NewNeuronId, #{signal_len => SensorGenotype#sensor_genotype.signal_input_length, tag => {sensor, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}), 
							%5) Create the connection from the new neuron to the random neuron
							genotype:add_synapses(Genotype, NewNeuronId, NeuronId, #{signal_len => 1, tag => {neuron, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}),
							Mutation = #{type => add_layer_neuron, neuron_id => NewNeuronId, connected_with => {SensorId, NeuronId}},
							{Genotype, Mutations ++ [Mutation]};
						%else
						_ ->
							%1) Get the index layer before the reference layer
							{InfLayers, _} = lists:splitwith(fun(N) -> N < LayerReference end, Layers),
							LastLayerBefeorReferenceLayer = lists:last(InfLayers),
							%2) Create the new index layer
							NewLayerIndex = (LastLayerBeforeReferenceLayer + LayerReference) / 2,
							%3) Create the new neuron
							NewNeuronId = genotype:add_neuron(Genotype, #{layer => NewLayerIndex, activation_function => ActivationFunction}),
							%4) Create the connection from a random neuron in the layer before the reference layer and the new neuron
							PredicateInf = fun(Element) -> is_record(Element, neuron_classic_genotype) andalso Element#neuron_classic_genotype.layer == LastLayerBefeorReferenceLayer end,
							SelectFun = fun(Neuron) -> Neuron#neuron_classic_genotype.id end,
							NeuronInfId = ?RANDCHOOSE(genotype:get_select_on_elements_filtered(Genotype, PredicateInf, SelectFun)),
							genotype:add_synapses(Genotype, NeuronInfId, NewNeuronId, #{signal_len => 1, tag => {neuron, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}),
							%5) Create the connection from the new neuron and a random neuron in the reference layer
							PredicateSup = fun(Element) -> is_record(Element, neuron_classic_genotype) andalso Element#neuron_classic_genotype.layer == LayerReference end,
							NeuronSupId = ?RANDCHOOSE(genotype:get_select_on_elements_filtered(Genotype, PredicateInf, SelectFun)),
							genotype:add_synapses(Genotype, NewNeuronId, NeuronSupId, #{signal_len => 1, tag => {neuron, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}),
							Mutation = #{type => add_layer_neuron, neuron_id => NewNeuronId, connected_with => {NeuronInfId, NeuronSupId}},
							{Genotype, Mutations ++ [Mutation]}
						end;
				% Insert after the reference layer
				1 -> 
					%1) Get the index layer after the reference layer.
					{_, SupLayers} = lists:splitwith(fun(N) -> N <= LayerReference end, Layers),
					LayerNextReferenceLayer = lists:last(SupLayers),
					%2) Create the new index layer
					NewLayerIndex = (LayerNextReferenceLayer + LayerReference) / 2,
					%3) Create the new neuron
					NewNeuronId = genotype:add_neuron(Genotype, #{layer => NewLayerIndex, activation_function => ActivationFunction}),
					%4) Create the connection from a random neuron in the layer reference and the new neuron
					PredicateInf = fun(Element) -> is_record(Element, neuron_classic_genotype) andalso Element#neuron_classic_genotype.layer == LayerReference end,
					SelectFun = fun(Neuron) -> Neuron#neuron_classic_genotype.id end,
					NeuronInfId = ?RANDCHOOSE(genotype:get_select_on_elements_filtered(Genotype, PredicateInf, SelectFun)),
					genotype:add_synapses(Genotype, NeuronInfId, NewNeuronId, #{signal_len => 1, tag => {neuron, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}),
					%5) Create the connection from the new neuron and a random neuron in the layer after reference layer
					PredicateSup = fun(Element) -> is_record(Element, neuron_classic_genotype) andalso Element#neuron_classic_genotype.layer == LayerNextReferenceLayer end,
					NeuronSupId = ?RANDCHOOSE(genotype:get_select_on_elements_filtered(Genotype, PredicateInf, SelectFun)),
					genotype:add_synapses(Genotype, NewNeuronId, NeuronSupId, #{signal_len => 1, tag => {neuron, neuron}, modulation_type => PlasticityModulator, connection_direction => forward}),
					Mutation = #{type => add_layer_neuron, neuron_id => NewNeuronId, connected_with => {NeuronInfId, NeuronSupId}},
					{Genotype, Mutations ++ [Mutation]}
			end
	end