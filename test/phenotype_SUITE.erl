-module(phenotype_SUITE).
-export([all/0]).
-export([add_cortex_with_phenotype_test/1, add_sensor_with_phenotype_test/1, add_actuator_with_phenotype_test/1, add_classic_with_phenotype_test/1,
		add_som_with_phenotype_test/1, add_synapse_with_phenotype_test/1,
		add_synapses_from_label_test/1, add_synapses_from_model_test/1, 
		add_neuron_som_from_model_test/1,
		add_neuron_classic_from_model_test/1,
		add_cortex_from_model_test/1,
		add_sensor_from_model_test/1,
		add_actuator_from_model_test/1,
        update_element_test/1,
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
-include("phenotype.hrl").

all() -> [add_cortex_with_phenotype_test, add_sensor_with_phenotype_test, add_actuator_with_phenotype_test, add_classic_with_phenotype_test,
		add_som_with_phenotype_test, add_synapse_with_phenotype_test,
		add_synapses_from_label_test, add_synapses_from_model_test, 
		add_neuron_som_from_model_test,
		add_neuron_classic_from_model_test,
		add_cortex_from_model_test,
		add_sensor_from_model_test,
		add_actuator_from_model_test,
        update_element_test,
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



%%% phenotype API TEST
add_cortex_with_phenotype_test(_) ->
	%phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype) when is_record(ElementPhenotype, cortex_phenotype),
	ok.
add_sensor_with_phenotype_test(_) ->
	%phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype) when is_record(ElementPhenotype, sensor_phenotype),
	ok.
add_actuator_with_phenotype_test(_) ->
	%phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype) when is_record(ElementPhenotype, actuator_phenotype),
	ok.
add_classic_with_phenotype_test(_) ->
	%phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype) when is_record(ElementPhenotype, neuron_classic_phenotype),
	ok.
add_som_with_phenotype_test(_) ->
	%phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype) when is_record(ElementPhenotype, neuron_som_phenotype),
	ok.
add_synapse_with_phenotype_test(_) ->
	%phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype) when is_record(ElementPhenotype, synapses_phenotype),
	ok.

add_synapses_from_label_test(_) ->
	%phenotype:add_synapses(Phenotype, IdFrom, IdTo, #{signal_len := SignalLength, tag := Tag, modulation_type := NeuroModulationType, connection_direction := ConnectionDirection}),
	ok.
add_synapses_from_model_test(_) ->
	%phenotype:add_synapses(Phenotype, IdFrom, IdTo, #{tag := Tag, weight := Weight, plasticity_modulation := Modulation, type := ConnectionDirection}),
	ok.

add_neuron_som_from_model_test(_) ->
	%phenotype:add_neuron(Phenotype, #{id := NeuronId, weight := Weight, coordinates := Coordinates, activation_function := ActivationFunction}) when Phenotype#phenotype.network_type == som,
	ok.
add_neuron_classic_from_model_test(_) ->
	%phenotype:add_neuron(Phenotype, #{id := NeuronId, bias := Bias, layer := Layer, activation_function := ActivationFunction}) when Phenotype#phenotype.network_type == classic,
	ok.

add_cortex_from_model_test(_) ->
	%phenotype:add_cortex(Phenotype, #{id := CortexId, fit_directives := FitDirectives, real_directives := RealDirectives}),
	ok.

add_sensor_from_model_test(_) ->
	%phenotype:add_sensor(Phenotype, #{id := SensorId, signal_input_length := SignalInputLen, fit_directives := FitDirectives, real_directives := RealDirectives}),
	ok.

add_actuator_from_model_test(_) ->
	%phenotype:add_actuator(Phenotype, #{id := ActuatorId, number_of_clients := NumberInputSignals, fit_directives := FitDirectives, real_directives := RealDirectives}),
	ok.

update_element_test(_) ->
    %phenotype:update_element(Phenotype, ElementId, Directive),
    ok.

get_elements_filtered_test(_) ->
	%phenotype:get_elements_filtered(Phenotype, Predicate),
	ok.

get_select_on_elements_filtered_test(_) ->
	%phenotype:get_select_on_elements_filtered(Phenotype, Predicate, SelectFunction),
	ok.

get_element_by_id_test(_) ->
	%phenotype:get_element_by_id(Phenotype, ElementId),
	ok.

get_sensors_test(_) ->
	%phenotype:get_sensors(Phenotype),
	ok.

get_sensors_ids_test(_) ->
	%phenotype:get_sensors_ids(Phenotype),
	ok.

get_actuators_test(_) ->
	%phenotype:get_actuators(Phenotype),
	ok.

get_actuators_ids_test(_) ->
	%phenotype:get_actuators_ids(Phenotype),
	ok.

get_neuron_ids_som_test(_) ->
	%phenotype:get_neurons_ids(Phenotype) when Phenotype#phenotype.network_type == som,
	ok.
get_neuron_ids_classic_test(_) ->
	%phenotype:get_neurons_ids(Phenotype) when Phenotype#phenotype.network_type == classic,
	ok.

get_neurons_som_test(_) ->
	%phenotype:get_neurons(Phenotype) when Phenotype#phenotype.network_type == som,
	ok.
get_neurons_classic_test(_) ->
	%phenotype:get_neurons(Phenotype) when Phenotype#phenotype.network_type == classic,
	ok.

get_cortex_test(_) ->
	%phenotype:get_cortex(Phenotype),
	ok.

get_cortex_id_test(_) ->
	%phenotype:get_cortex_id(Phenotype),
	ok.

get_synapses_test(_) ->
	%phenotype:get_synapse(Phenotype, IdFrom, IdTo),
	ok.

delete_element_test(_) ->
	%phenotype:delete_element(Phenotype, ElementId),
	ok.

delete_synapse_test(_) ->
	%phenotype:delete_synapse(Phenotype, IdFrom, idTo),
	ok.
%%%