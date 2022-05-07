-module(genotype_SUITE).
-export([all/0]).
-export([create_som/1, create_ffnn/1, create_rnn/1, create_rnn2/1]).
-export([add_cortex_with_genotype_test/1, add_sensor_with_genotype_test/1, add_actuator_with_genotype_test/1, add_classic_with_genotype_test/1,
		add_som_with_genotype_test/1, add_synapse_with_genotype_test/1,
		add_synapses_from_label_test/1, add_synapses_from_model_test/1, 
		add_neuron_som_from_label_test/1, add_neuron_som_from_model_test/1,
		add_neuron_classic_from_label_test/1, add_neuron_classic_from_model_test/1,
		add_cortex_from_label_test/1, add_cortex_from_model_test/1,
		add_sensor_from_label_test/1, add_sensor_from_model_test/1,
		add_actuator_from_label_test/1, add_actuator_from_model_test/1,
		get_elements_filtered_test/1, get_select_on_elements_filtered_test/1,
		get_element_by_id_test/1,
		get_cortex_id_test/1, get_cortex_test/1,
		get_sensors_ids_test/1, get_sensors_test/1,
		get_actuators_ids_test/1, get_actuators_test/1, 
		get_neuron_ids_som_test/1, get_neurons_som_test/1,
		get_neuron_ids_classic_test/1, get_neurons_classic_test/1,
		get_synapses_test/1,
		delete_element_test/1, delete_synapse_test/1
	]).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("genotype.hrl").

all() -> [create_som, create_ffnn, create_rnn, create_rnn2,
		add_cortex_with_genotype_test, add_sensor_with_genotype_test, add_actuator_with_genotype_test, add_classic_with_genotype_test,
		add_som_with_genotype_test, add_synapse_with_genotype_test,
		add_synapses_from_label_test, add_synapses_from_model_test, 
		add_neuron_som_from_label_test, add_neuron_som_from_model_test,
		add_neuron_classic_from_label_test, add_neuron_classic_from_model_test,
		add_cortex_from_label_test, add_cortex_from_model_test,
		add_sensor_from_label_test, add_sensor_from_model_test,
		add_actuator_from_label_test, add_actuator_from_model_test,
		get_elements_filtered_test, get_select_on_elements_filtered_test,
		get_element_by_id_test,
		get_cortex_id_test, get_cortex_test,
		get_sensors_ids_test, get_sensors_ids_test,
		get_actuators_ids_test, get_actuators_test, 
		get_neuron_ids_som_test, get_neurons_som_test,
		get_neuron_ids_classic_test, get_neurons_classic_test,
		get_synapses_test,
		delete_element_test, delete_synapse_test
	].



%%% GENOTYPE API TEST
add_cortex_with_genotype_test(_) ->
	Genotype = genotype:new(classic),
	ElementGenotype = #cortex_genotype{id = 1},
	genotype:add_element_with_genotype(Genotype, ElementGenotype),
	%Check if cortex is added
	?assert(digraph:vertex(Genotype#genotype.network, 1) /= false).

add_sensor_with_genotype_test(_) ->
	Genotype = genotype:new(classic),
	ElementGenotype = #sensor_genotype{id = 1},
	genotype:add_element_with_genotype(Genotype, ElementGenotype),
	%Check if sensor is added
	?assert(digraph:vertex(Genotype#genotype.network, 1) /= false).

add_actuator_with_genotype_test(_) ->
	Genotype = genotype:new(classic),
	ElementGenotype = #actuator_genotype{id = 1},
	genotype:add_element_with_genotype(Genotype, ElementGenotype),
	%Check if actuator is added
	?assert(digraph:vertex(Genotype#genotype.network, 1) /= false).

add_classic_with_genotype_test(_) ->
	Genotype = genotype:new(classic),
	ElementGenotype = #neuron_classic_genotype{id = 1},
	genotype:add_element_with_genotype(Genotype, ElementGenotype),
	%Check if neuron is added
	?assert(digraph:vertex(Genotype#genotype.network, 1) /= false).

add_som_with_genotype_test(_) ->
	Genotype = genotype:new(som),
	ElementGenotype = #neuron_som_genotype{id = 1},
	genotype:add_element_with_genotype(Genotype, ElementGenotype),
	%Check if neuron is added
	?assert(digraph:vertex(Genotype#genotype.network, 1) /= false).

add_synapse_with_genotype_test(_) ->
	Genotype = get_default_genotype(),
	SynapseGenotype = #synapses{id_from = 1, id_to = 2},
	genotype:add_element_with_genotype(Genotype, SynapseGenotype),
	%Check if edge is added
	?assert(digraph:edge(Genotype#genotype.network, {1,2}) /= false).


add_synapses_from_label_test(_) ->
	Genotype = get_default_genotype(),
	genotype:add_synapses(Genotype, 1, 2, #{signal_len => 3, tag => {sensor, neuron}, modulation_type => hebbian, connection_direction => forward}),
	{{1, 2}, 1, 2, Edge} = digraph:edge(Genotype#genotype.network, {1, 2}),
	%Check if edge is added
	?assert(Edge /= false),
	%Check if the weight is of length 3
	?assert(length(Edge#synapses.weight) == 3),
	%Check if the modulation  is hebbian of length 3
	Modulation = Edge#synapses.plasticity_modulation,
	?assert(element(1, Modulation) == hebbian),
	?assert(length(element(2, Modulation)) == 3).

add_synapses_from_model_test(_) ->
	Genotype = get_default_genotype(),
	genotype:add_synapses(Genotype, #{id_from => 1, id_to => 2, tag => {sensor, neuron}, weight => [1,2,3], plasticity_modulation => none, type => forward}),
	%Check if edge is added
	{{1, 2}, 1, 2, Edge} = digraph:edge(Genotype#genotype.network, {1,2}),
	?assert(Edge /= false).

add_neuron_som_from_label_test(_) ->
	Genotype = genotype:new(som),
	genotype:add_neuron(Genotype, #{signal_len => 4, coordinates => {0,0}, activation_function => manhattan}),
	Vertices = digraph:vertices(Genotype#genotype.network),
	%Check if the neuron is added
	?assert(length(Vertices) == 1),
	%Check if the neuron som have a signal vector of length 4
	[Vertex] = Vertices,
	VertexLabel = digraph:vertex(Genotype#genotype.network, Vertex),
	?assert(VertexLabel /= false),
	{Vertex, Label} = VertexLabel,
	?assert(length(Label#neuron_som_genotype.weight) == 4).

add_neuron_classic_from_label_test(_) ->
	Genotype = genotype:new(classic),
	genotype:add_neuron(Genotype, #{layer => 0, activation_function => identity}),
	Vertices = digraph:vertices(Genotype#genotype.network),
	%Check if the neuron is added
	?assert(length(Vertices) == 1).

add_neuron_som_from_model_test(_) ->
	Genotype = genotype:new(som),
	genotype:add_neuron(Genotype, #{id => 1, weight => [3,5], coordinates => {0,0}, cluster => 0, activation_function => manhattan_distance}),
	%Check if neuron is added
	?assert(digraph:vertex(Genotype#genotype.network, 1) /= false).

add_neuron_classic_from_model_test(_) ->
	Genotype = genotype:new(classic),
	genotype:add_neuron(Genotype, #{id => 1, bias => 3, layer => 1, activation_function => rectifier}),
	Vertices = digraph:vertices(Genotype#genotype.network),
	%Check if the neuron is added
	?assert(length(Vertices) == 1).

add_cortex_from_label_test(_) ->
	Genotype = genotype:new(classic),
	genotype:add_cortex(Genotype, #{fit_directives => [], real_directives => []}),
	Vertices = digraph:vertices(Genotype#genotype.network),
	%Check if the neuron is added
	?assert(length(Vertices) == 1).
add_cortex_from_model_test(_) ->
	Genotype = genotype:new(classic),
	genotype:add_cortex(Genotype, #{id => 1, fit_directives => [], real_directives => []}),
	Vertices = digraph:vertices(Genotype#genotype.network),
	%Check if the cortex is added
	?assert(length(Vertices) == 1).


add_sensor_from_label_test(_) ->
	Genotype = genotype:new(classic),
	genotype:add_sensor(Genotype, #{signal_input_length => 5, fit_directives => [], real_directives => []}),
	Vertices = digraph:vertices(Genotype#genotype.network),
	%Check if the neuron is added
	?assert(length(Vertices) == 1).
add_sensor_from_model_test(_) ->
	Genotype = genotype:new(classic),
	genotype:add_sensor(Genotype, #{id => 1, signal_input_length => 5, fit_directives => [], real_directives => []}),
	Vertices = digraph:vertices(Genotype#genotype.network),
	%Check if the sensor is added
	?assert(length(Vertices) == 1).

add_actuator_from_label_test(_) ->
	Genotype = genotype:new(classic),
	genotype:add_actuator(Genotype, #{number_of_clients => 4, fit_directives => [], real_directives => []}),
	Vertices = digraph:vertices(Genotype#genotype.network),
	%Check if the actuator is added
	?assert(length(Vertices) == 1).
add_actuator_from_model_test(_) ->
	Genotype = genotype:new(classic),
	genotype:add_actuator(Genotype, #{id => 1, number_of_clients => 4, fit_directives => [], real_directives => []}),
	Vertices = digraph:vertices(Genotype#genotype.network),
	%Check if the actuator is added
	?assert(length(Vertices) == 1).

get_elements_filtered_test(_) ->
	Genotype = get_default_genotype(),
	Predicate = fun(Element) -> case is_record(Element, neuron_classic_genotype) of true -> true; false -> false end end,
	ElementsFiltered = genotype:get_elements_filtered(Genotype, Predicate),
	%Check if there is only the neuron 
	?assert(length(ElementsFiltered) == 1),
	[Neuron] = ElementsFiltered,
	?assert(is_record(Neuron, neuron_classic_genotype)).


get_select_on_elements_filtered_test(_) ->
	Genotype = get_default_genotype(),
	Predicate = fun(Element) -> case is_record(Element, sensor_genotype) of true -> true; false -> false end end,
	SelectFunction = fun(#sensor_genotype{signal_input_length = X}) -> X end, 
	ElementsFiltered = genotype:get_select_on_elements_filtered(Genotype, Predicate, SelectFunction),
	%Check if there is only the sensor 
	?assert(length(ElementsFiltered) == 1),
	[Signal] = ElementsFiltered,
	?assert(Signal == 3).

get_element_by_id_test(_) ->
	Genotype = get_default_genotype(),
	Element = genotype:get_element_by_id(Genotype, 2),
	%Check if is the neuron
	?assert(is_record(Element, neuron_classic_genotype)),
	%Check the bias of the neuron
	?assert(Element#neuron_classic_genotype.bias == 4).

get_sensors_test(_) ->
	Genotype = get_default_genotype(),
	Sensors = genotype:get_sensors(Genotype),
	%Check if there is the sensor
	?assert(length(Sensors) == 1),
	[Sensor] = Sensors,
	%Check if the element is a sensor
	?assert(is_record(Sensor, sensor_genotype)).

get_sensors_ids_test(_) ->
	Genotype = get_default_genotype(),
	SensorsIds = genotype:get_sensors_ids(Genotype),
	%Check if there is the sensor
	?assert(length(SensorsIds) == 1),
	[SensorId] = SensorsIds,
	%Check if the element is a sensor
	?assert(SensorId == 1).

get_actuators_test(_) ->
	Genotype = get_default_genotype(),
	Actuators = genotype:get_actuators(Genotype),
	%Check if there is the actuator
	?assert(length(Actuators) == 1),
	[Actuator] = Actuators,
	%Check if the element is a actuator
	?assert(is_record(Actuator, actuator_genotype)).

get_actuators_ids_test(_) ->
	Genotype = get_default_genotype(),
	ActuatorsIds = genotype:get_actuators_ids(Genotype),
	%Check if there is the actuator
	?assert(length(ActuatorsIds) == 1),
	[ActuatorId] = ActuatorsIds,
	%Check if the element is a actuator
	?assert(ActuatorId == 4).

get_neuron_ids_som_test(_) ->
	Genotype = get_default_genotype(),
	NeuronsSomIds = genotype:get_neuron_ids(Genotype),
	%Check if only neuron classic is taken, genotype is type classic!
	?assert(length(NeuronsSomIds) == 1).

get_neuron_ids_classic_test(_) ->
	Genotype = get_default_genotype(),
	NeuronsIds = genotype:get_neuron_ids(Genotype),
	%Check if there is the neuron classic
	?assert(length(NeuronsIds) == 1),
	[NeuronId] = NeuronsIds,
	%Check if the element is the neuron som
	?assert(NeuronId == 2).

get_neurons_som_test(_) ->
	Genotype = get_default_genotype(),
	NeuronsSom = genotype:get_neuron_ids(Genotype),
	%Check if only one neurons are take(only the classic), genotype is type classic!
	?assert(length(NeuronsSom) == 1).
get_neurons_classic_test(_) ->
	Genotype = get_default_genotype(),
	Neurons = genotype:get_neurons(Genotype),
	%Check if there is the neuron classic
	?assert(length(Neurons) == 1),
	[Neuron] = Neurons,
	%Check if the element is the neuron som
	?assert(is_record(Neuron, neuron_classic_genotype)).

get_cortex_test(_) ->
	Genotype = get_default_genotype(),
	Cortex = genotype:get_cortex(Genotype),
	?assert(is_record(Cortex, cortex_genotype)).

get_cortex_id_test(_) ->
	Genotype = get_default_genotype(),
	CortexId = genotype:get_cortex_id(Genotype),
	?assert(CortexId == 5).

get_synapses_test(_) ->
	Genotype = get_default_genotype(),
	Synapse = genotype:get_synapses(Genotype, 2, 4),
	%Check if there is the synapse
	?assert(is_record(Synapse, synapses)),
	%Check if is the synapse we want
	?assert(Synapse#synapses.id_from == 2 andalso Synapse#synapses.id_to == 4).

delete_element_test(_) ->
	Genotype = get_default_genotype(),
	genotype:delete_element(Genotype, 5),
	%Check if there is no more the cortex
	?assert(digraph:vertex(Genotype#genotype.network, 5) == false). 

delete_synapse_test(_) ->
	Genotype = get_default_genotype(),
	genotype:delete_synapse(Genotype, 2, 4),
	%Check if there is no more the edge
	?assert(digraph:edge(Genotype#genotype.network, {2,4}) == false). 
%%%

create_som(_)->
    SensorSpec = {3, [], []},
	ActuatorSpec = {[], [{som_service, get_BMU_cluster, []}]},
	CortexSpec = {[{fun(List) -> hd(List) end,[]}], [{fun(List) -> hd(List) end,[]}]},
    Genotype = genotype:create_NN({som, euclidean}, SensorSpec, ActuatorSpec, CortexSpec, {3, 4}),
	print_genotype(Genotype).

create_ffnn(_) -> 
	SensorSpec = {3, [], []},
	ActuatorSpec ={1, [], []},
	CortexSpec = {[{fun(List) -> hd(List) end, []}], [{fun(List) -> hd(List) end,[]}]},
    Genotype = genotype:create_NN({ffnn, sigmund, hebbian}, SensorSpec, ActuatorSpec, CortexSpec, [3, 4]),
	print_genotype(Genotype).

create_rnn(_) -> 
	SensorSpec = {4, [], []},
	ActuatorSpec={1, [], []},
	CortexSpec = {[{fun(List) -> hd(List) end,[]}], [{fun(List) -> hd(List) end,[]}]},
    Genotype = genotype:create_NN({rnn, identity, none}, SensorSpec, ActuatorSpec, CortexSpec, [2, 2, 2]),
	print_genotype(Genotype).

create_rnn2(_) -> 
	SensorSpec = {4,[],[]},
	ActuatorSpec = {1, [],[]},
	CortexSpec = {[{fun(List) -> hd(List) end,[]}], [{fun(List) -> hd(List) end,[]}]},
    Genotype = genotype:create_NN({{rnn, 1}, identity, none}, SensorSpec, ActuatorSpec, CortexSpec, [2, 2, 2]),
	print_genotype(Genotype).


print_genotype(Genotype) ->
	Vertices = digraph:vertices(Genotype#genotype.network),
	Network = get_node_and_connection(Genotype#genotype.network, Vertices, []),
	io:fwrite("Net: ~p~n",[Network]).

get_node_and_connection(_, [], Acc) -> Acc;
get_node_and_connection(Graph, [El | T ], Acc) ->
	{El, Label} = digraph:vertex(Graph, El),
	Entranti = [ get_edge_label(in, Graph, digraph:edge(Graph, Edge))||Edge <- digraph:in_edges(Graph, El)],
	Uscenti = [ get_edge_label(out, Graph, digraph:edge(Graph, Edge))||Edge <- digraph:out_edges(Graph, El)],
	NewAcc = [#{label => Label, entranti => Entranti, uscenti => Uscenti} | Acc ],
	get_node_and_connection(Graph, T, NewAcc).

get_edge_label(in, Graph, {_, V1, _,_}) ->
	digraph:vertex(Graph, V1);
get_edge_label(out, Graph, {_, _, V2, _}) ->
	digraph:vertex(Graph, V2).

%%%Private function for this test module
get_default_genotype() ->
	Genotype = genotype:new(classic),
	genotype:add_element_with_genotype(Genotype, #sensor_genotype{id = 1, signal_input_length = 3}),
	genotype:add_element_with_genotype(Genotype, #neuron_classic_genotype{id = 2, bias = 4}),
	genotype:add_element_with_genotype(Genotype, #neuron_som_genotype{id = 3, weight = 5}),
	genotype:add_element_with_genotype(Genotype, #actuator_genotype{id = 4, number_of_clients = 1}),
	genotype:add_element_with_genotype(Genotype, #cortex_genotype{id = 5}),
	genotype:add_element_with_genotype(Genotype, #synapses{id_from = 2, id_to = 4}),
	Genotype.
%%%