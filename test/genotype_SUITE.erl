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
	%genotype:add_element_with_genotype(Genotype, ElementGenotype) when when is_record(ElementGenotype, cortex_genotype),
	ok.
add_sensor_with_genotype_test(_) ->
	%genotype:add_element_with_genotype(Genotype, ElementGenotype) when when is_record(ElementGenotype, sensor_genotype),
	ok.
add_actuator_with_genotype_test(_) ->
	%genotype:add_element_with_genotype(Genotype, ElementGenotype) when when is_record(ElementGenotype, actuator_genotype),
	ok.
add_classic_with_genotype_test(_) ->
	%genotype:add_element_with_genotype(Genotype, ElementGenotype) when when is_record(ElementGenotype, neuron_classic_genotype),
	ok.
add_som_with_genotype_test(_) ->
	%genotype:add_element_with_genotype(Genotype, ElementGenotype) when when is_record(ElementGenotype, neuron_som_genotype),
	ok.
add_synapse_with_genotype_test(_) ->
	%genotype:add_element_with_genotype(Genotype, ElementGenotype) when when is_record(ElementGenotype, synapses_genotype),
	ok.

add_synapses_from_label_test(_) ->
	%genotype:add_synapses(Genotype, IdFrom, IdTo, #{signal_len := SignalLength, tag := Tag, modulation_type := NeuroModulationType, connection_direction := ConnectionDirection}),
	ok.
add_synapses_from_model_test(_) ->
	%genotype:add_synapses(Genotype, IdFrom, IdTo, #{id_from := IdFrom, id_to := IdTo, tag := Tag, weight := Weight, plasticity_modulation := Modulation, type := ConnectionDirection}),
	ok.

add_neuron_som_from_label_test(_) ->
	%genotype:add_neuron(Genotype, #{signal_len := SignalLength, coordinates := Coordinates, activation_function := ActivationFunction}) when Genotype#genotype.network_type == som,
	ok.
add_neuron_classic_from_label_test(_) ->
	%genotype:add_neuron(Genotype, #{layer := Layer, activation_function := ActivationFunction}) when Genotype#genotype.network_type == classic,
	ok.

add_neuron_som_from_model_test(_) ->
	%genotype:add_neuron(Genotype, #{id := NeuronId, weight := Weight, coordinates := Coordinates, cluster := Cluster, activation_function := ActivationFunction}) when Genotype#genotype.network_type == som,
	ok.
add_neuron_classic_from_model_test(_) ->
	%genotype:add_neuron(Genotype, #{id := NeuronId, bias := Bias, layer := Layer, activation_function := ActivationFunction}) when Genotype#genotype.network_type == classic,
	ok.

add_cortex_from_label_test(_) ->
	%genotype:add_cortex(Genotype, #{fit_directives := FitDirectives, real_directives := RealDirectives}),
	ok.
add_cortex_from_model_test(_) ->
	%genotype:add_cortex(Genotype, #{id := CortexId, fit_directives := FitDirectives, real_directives := RealDirectives}),
	ok.

add_sensor_from_label_test(_) ->
	%genotype:add_sensor(Genotype, #{signal_input_length := SignalInputLen, fit_directives := FitDirectives, real_directives := RealDirectives}),
	ok.
add_sensor_from_model_test(_) ->
	%genotype:add_sensor(Genotype, #{id := SensorId, signal_input_length := SignalInputLen, fit_directives := FitDirectives, real_directives := RealDirectives}),
	ok.

add_actuator_from_label_test(_) ->
	%genotype:add_actuator(Genotype, #{number_of_clients := NumberInputSignals, fit_directives := FitDirectives, real_directives := RealDirectives}),
	ok.
add_actuator_from_model_test(_) ->
	%genotype:add_actuator(Genotype, #{id := ActuatorId, number_of_clients := NumberInputSignals, fit_directives := FitDirectives, real_directives := RealDirectives}),
	ok.

get_elements_filtered_test(_) ->
	%genotype:get_elements_filtered(Genotype, Predicate),
	ok.

get_select_on_elements_filtered_test(_) ->
	%genotype:get_select_on_elements_filtered(Genotype, Predicate, SelectFunction),
	ok.

get_element_by_id_test(_) ->
	%genotype:get_element_by_id(Genotype, ElementId),
	ok.

get_sensors_test(_) ->
	%genotype:get_sensors(Genotype),
	ok.

get_sensors_ids_test(_) ->
	%genotype:get_sensors_ids(Genotype),
	ok.

get_actuators_test(_) ->
	%genotype:get_actuators(Genotype),
	ok.

get_actuators_ids_test(_) ->
	%genotype:get_actuators_ids(Genotype),
	ok.

get_neuron_ids_som_test(_) ->
	%genotype:get_neurons_ids(Genotype) when Genotype#genotype.network_type == som,
	ok.
get_neuron_ids_classic_test(_) ->
	%genotype:get_neurons_ids(Genotype) when Genotype#genotype.network_type == classic,
	ok.

get_neurons_som_test(_) ->
	%genotype:get_neurons(Genotype) when Genotype#genotype.network_type == som,
	ok.
get_neurons_classic_test(_) ->
	%genotype:get_neurons(Genotype) when Genotype#genotype.network_type == classic,
	ok.

get_cortex_test(_) ->
	%genotype:get_cortex(Genotype),
	ok.

get_cortex_id_test(_) ->
	%genotype:get_cortex_id(Genotype),
	ok.

get_synapses_test(_) ->
	%genotype:get_synapse(Genotype, IdFrom, IdTo),
	ok.

delete_element_test(_) ->
	%genotype:delete_element(Genotype, ElementId),
	ok.

delete_synapse_test(_) ->
	%genotype:delete_synapse(Genotype, IdFrom, idTo),
	ok.
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

