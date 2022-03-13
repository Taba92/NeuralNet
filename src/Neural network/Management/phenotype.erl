-module(phenotype).
-export([get_select_on_elements_filtered/3, get_elements_filtered/2, get_element_by_id/2]).
-export([get_sensors/1, get_sensors_ids/1, get_actuators/1, get_actuators_ids/1, get_neurons/1, get_neuron_ids/1, get_cortex/1, get_cortex_id/1, get_synapses/3]).
-export([update_element/3]).
-export([add_neuron/2, add_actuator/2, add_sensor/2, add_cortex/2, add_element_with_phenotype/2, add_synapses/4]).
-export([delete_element/2, delete_synapse/3]).
-include("utils.hrl").
-include("phenotype.hrl").


%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_elements_filtered(Phenotype, Predicate) ->
	Dets = Phenotype#phenotype.elements_dets,
	% In the phenotype table elements data are stored as a tuple {Id, NodeType, IsRemoteOrLocal}
	Filter = fun({ElementId, _, _}, Acc) ->
				ElementPhenotype = get_element_by_id(Phenotype, ElementId),
				case Predicate(ElementPhenotype) of
					true -> [ElementPhenotype | Acc];
					false -> Acc
				end
			end,
	dets:foldl(Filter, [], Dets).

get_select_on_elements_filtered(Phenotype, Predicate, SelectFunction) ->
	Dets = Phenotype#phenotype.elements_dets,
	% In the phenotype table elements data are stored as a tuple {Id, NodeType, IsRemoteOrLocal}
	Filter = fun({ElementId, _, _}, Acc) ->
				ElementPhenotype = get_element_by_id(Phenotype, ElementId),
				case Predicate(ElementPhenotype) of
					true -> [SelectFunction(ElementPhenotype) | Acc];
					false -> Acc
				end
			end,
	dets:foldl(Filter, [], Dets).

get_element_by_id(_, ElementId) ->
	ElementPhenotype = gen_server:call(ElementId, get, infinity),
	ElementPhenotype.

get_sensors(Phenotype) ->
	Predicate = fun(Element) -> is_record(Element, sensor_phenotype) end,
	get_elements_filtered(Phenotype, Predicate).

get_sensors_ids(Phenotype) ->
	Predicate = fun(Element) -> is_record(Element, sensor_phenotype) end,
	Select = fun(#sensor_phenotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Phenotype, Predicate, Select).

get_actuators(Phenotype) ->
	Predicate = fun(El) -> is_record(El, actuator_phenotype) end,
	get_elements_filtered(Phenotype, Predicate).

get_actuators_ids(Phenotype) ->
	Predicate = fun(El) -> is_record(El, actuator_phenotype) end,
	Select = fun(#actuator_phenotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Phenotype, Predicate, Select).

get_cortex(Phenotype) ->
	Predicate = fun(El) -> is_record(El, cortex_phenotype) end,
	[Cortex] = get_elements_filtered(Phenotype, Predicate),
	Cortex.

get_cortex_id(Phenotype) ->
	Cortex = get_cortex(Phenotype),
	#cortex_phenotype{id = Id} = Cortex,
	Id.

get_neuron_ids(Phenotype) when Phenotype#phenotype.network_type == som ->
	Predicate = fun(El) -> is_record(El, neuron_som_phenotype) end,
	Select = fun(#neuron_som_phenotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Phenotype, Predicate, Select);
get_neuron_ids(Phenotype) when Phenotype#phenotype.network_type == classic->
	Predicate = fun(El) -> is_record(El, neuron_classic_phenotype) end,
	Select = fun(#neuron_classic_phenotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Phenotype, Predicate, Select).

get_neurons(Phenotype) when Phenotype#phenotype.network_type == som->
	Predicate = fun(El) -> is_record(El, neuron_som_phenotype) end,
	get_elements_filtered(Phenotype, Predicate);
get_neurons(Phenotype) when Phenotype#phenotype.network_type == classic->
	Predicate = fun(El) -> is_record(El, neuron_classic_phenotype) end,
	get_elements_filtered(Phenotype, Predicate).

get_synapses(_Phenotype, _IdFrom, _IdTo) ->
	%The node receiver hold the informations about synapse
	throw(to_be_implemented).

%Given existing phenotype node data, start the node process, adding the id to the phenotype data structure
add_element_with_phenotype(Phenotype, NodePhenotype) when is_record(NodePhenotype, cortex_phenotype) ->
	#phenotype{elements_dets = Dets} = Phenotype,
	#cortex_phenotype{id = Id} = NodePhenotype,
	dets:insert(Dets, {Id, cortex, local}),
	?CORTEX_MODULE:init(NodePhenotype);
add_element_with_phenotype(Phenotype, NodePhenotype) when is_record(NodePhenotype, sensor_phenotype) ->
	#phenotype{elements_dets = Dets} = Phenotype,
	#sensor_phenotype{id = Id} = NodePhenotype,
	dets:insert(Dets, {Id, sensor, local}),
	?SENSOR_MODULE:init(NodePhenotype);
add_element_with_phenotype(Phenotype, NodePhenotype) when is_record(NodePhenotype, actuator_phenotype) ->
	#phenotype{elements_dets = Dets} = Phenotype,
	#actuator_phenotype{id = Id} = NodePhenotype,
	dets:insert(Dets, {Id, actuator, local}),
	?ACTUATOR_MODULE:init(NodePhenotype);
add_element_with_phenotype(Phenotype, NodePhenotype) when is_record(NodePhenotype, neuron_classic_phenotype) ->
	#phenotype{elements_dets = Dets} = Phenotype,
	#neuron_classic_phenotype{id = Id} = NodePhenotype,
	dets:insert(Dets, {Id, neuron_classic, local}),
	?NEURON_CLASSIC_MODULE:init(NodePhenotype);
add_element_with_phenotype(Phenotype, NodePhenotype) when is_record(NodePhenotype, neuron_som_phenotype) ->
	#phenotype{elements_dets = Dets} = Phenotype,
	#neuron_som_phenotype{id = Id} = NodePhenotype,
	dets:insert(Dets, {Id, neuron_som, local}),
	?NEURON_SOM_MODULE:init(NodePhenotype).


%Create element using existing model, derived from own genotype in base of model type
add_cortex(Phenotype, #{id := CortexId, fit_directives := FitDirectives, real_directives := RealDirectives}) ->
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:insert(Dets, {CortexId, cortex, local}),
	CortexPhenotype = #cortex_phenotype{id = CortexId, fit_directives = FitDirectives, real_directives = RealDirectives},
	?CORTEX_MODULE:init(CortexPhenotype),
	CortexId.

add_sensor(Phenotype, #{id := SensorId, signal_input_length := InputLength, fit_directives := FitDirectives, real_directives := RealDirectives}) ->
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:insert(Dets, {SensorId, sensor, local}),
	SensorPhenotype = #sensor_phenotype{id = SensorId, signal_input_length = InputLength, fit_directives = FitDirectives, real_directives = RealDirectives},
	?SENSOR_MODULE:init(SensorPhenotype),
	SensorId.

add_actuator(Phenotype, #{id := ActuatorId, number_of_clients := NumClients, fit_directives := FitDirectives, real_directives := RealDirectives}) ->
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:insert(Dets, {ActuatorId, actuator, local}),
	ActuatorPhenotype = #actuator_phenotype{id = ActuatorId, number_of_clients = NumClients, fit_directives = FitDirectives, real_directives = RealDirectives},
	?ACTUATOR_MODULE:init(ActuatorPhenotype),
	ActuatorId.

add_neuron(Phenotype, #{id := NeuronId, coordinates := Coordinates, weight := Weight, activation_function := ActivationFunction}) when Phenotype#phenotype.network_type == som ->
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:insert(Dets, {NeuronId, neuron_som, local}),
	NeuronSomPhenotype = #neuron_som_phenotype{id = NeuronId, weight = Weight, coordinates = Coordinates, activation_function = ActivationFunction},
	?NEURON_SOM_MODULE:init(NeuronSomPhenotype),
	NeuronId;
add_neuron(Phenotype, #{id := NeuronId, layer := Layer, bias := Bias, activation_function := ActivationFunction}) when Phenotype#phenotype.network_type == classic ->
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:insert(Dets, {NeuronId, neuron_classic, local}),
	NeuronClassicPhenotype = #neuron_classic_phenotype{id = NeuronId, bias = Bias, layer = Layer, activation_function = ActivationFunction},
	?NEURON_CLASSIC_MODULE:init(NeuronClassicPhenotype),
	NeuronId.

%Inizialize synapses with new parameters
add_synapses(Phenotype, IdFrom, IdTo, #{signal_len := SignalLength, tag := Tag, modulation_type := NeuroModulationType, connection_direction := ConnectionDirection}) ->
	Weight = utils:get_random_list(SignalLength),
	Modulation = plasticity:get_plasticity(Weight, NeuroModulationType),
	SynapseLabel = #{weight => Weight, tag => Tag, modulation => Modulation, connection_direction => ConnectionDirection},
	add_synapses(Phenotype, IdFrom, IdTo, SynapseLabel);
%Initialize synapses with parameters already present
add_synapses(Phenotype, IdFrom, IdTo, #{weight := Weight, tag := Tag, modulation := Modulation, connection_direction := ConnectionDirection}) ->
	%Create the phenotype of the sinapses
	SinapsesPhenotype = {IdFrom, IdTo, Tag, Weight, Modulation, ConnectionDirection},
	%The emanate node will store only the id of the receiver (IdTo)
	update_element(Phenotype, IdFrom, {add_synapses, SinapsesPhenotype}),
	% The incident node will store the information for process inbound signals from IdFrom
	update_element(Phenotype, IdTo, {add_synapses, SinapsesPhenotype}),
	{IdFrom, IdTo}.


%Send directive to the element with id = ElementId
update_element(_, ElementId, DirectiveLabel) ->
	gen_server:call(ElementId, {update, DirectiveLabel}, infinity),
	ok.

delete_element(Phenotype, ElementId) ->
	%NB: This method delete ONLY the node. Every inbound and outbound sinapses of this node are maintained on others the connected nodes!
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:delete(Dets, ElementId),
	gen_server:stop(ElementId, normal, infinity),
	ok.

delete_synapse(Phenotype, IdFrom, IdTo) ->
	%Given the phenotype, get the type of the element
	GetTypeFun = fun (ElementPhenotype) when is_record(ElementPhenotype, cortex_phenotype) ->
						cortex;
					  (ElementPhenotype) when is_record(ElementPhenotype, sensor_phenotype) ->
						sensor;
					(ElementPhenotype) when is_record(ElementPhenotype, actuator_phenotype) ->
						actuator;
					(ElementPhenotype) when is_record(ElementPhenotype, neuron_classic_phenotype) ->
						neuron_classic;
					(ElementPhenotype) when is_record(ElementPhenotype, neuron_som_phenotype) ->
						neuron_som
				end,
	%1) Get the tag of the synapse
	FromPhenotype = get_element_by_id(Phenotype, IdFrom),
	ToPhenotype = get_element_by_id(Phenotype, IdTo),
	Tag = {GetTypeFun(FromPhenotype), GetTypeFun(ToPhenotype)},
	%2) Delete the synapse
	gen_server:call(IdFrom, {delete_synapses, IdFrom, IdTo, Tag}),
	gen_server:call(IdTo, {delete_synapses, IdFrom, IdTo, Tag}),
	ok.
