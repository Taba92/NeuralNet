-module(phenotype_SUITE).
-export([all/0]).
-export([add_cortex_with_phenotype_test/1, add_sensor_with_phenotype_test/1, add_actuator_with_phenotype_test/1, add_classic_with_phenotype_test/1,
		add_som_with_phenotype_test/1,
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
-include_lib("stdlib/include/assert.hrl").
-include("phenotype.hrl").

all() -> [add_cortex_with_phenotype_test, add_sensor_with_phenotype_test, add_actuator_with_phenotype_test, add_classic_with_phenotype_test,
		add_som_with_phenotype_test,
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
	Phenotype = phenotype:new(classic),
	ElementPhenotype = #cortex_phenotype{id = test},
	phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype),
	%Check if cortex is added
	?assert(whereis(test) /= undefined),
	phenotype:delete(Phenotype).

add_sensor_with_phenotype_test(_) ->
	Phenotype = phenotype:new(classic),
	ElementPhenotype = #sensor_phenotype{id = test},
	phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype),
	%Check if sensor is added
	?assert(whereis(test) /= undefined),
	phenotype:delete(Phenotype).

add_actuator_with_phenotype_test(_) ->
	Phenotype = phenotype:new(classic),
	ElementPhenotype = #actuator_phenotype{id = '1'},
	phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype),
	%Check if actuator is added
	?assert(whereis('1') /= undefined),
	phenotype:delete(Phenotype).

add_classic_with_phenotype_test(_) ->
	Phenotype = phenotype:new(classic),
	ElementPhenotype = #neuron_classic_phenotype{id = '1', recurrent_input_elements_data = []},
	phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype),
	%Check if neuron classic is added
	?assert(whereis('1') /= undefined),
	phenotype:delete(Phenotype).

add_som_with_phenotype_test(_) ->
	Phenotype = phenotype:new(som),
	ElementPhenotype = #neuron_som_phenotype{id = '1'},
	phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype),
	%Check if neuron som is added
	?assert(whereis('1') /= undefined),
	phenotype:delete(Phenotype).

add_synapses_from_label_test(_) ->
	Phenotype = get_default_phenotype(),
	phenotype:add_synapses(Phenotype, '1', '2', #{signal_len => 3, tag => {sensor, neuron}, modulation_type => hebbian, connection_direction => forward}),
	#sensor_phenotype{output_elements_ids = Ids} = gen_server:call('1', get),
	#neuron_classic_phenotype{input_elements_data = Data, recurrent_input_elements_data = []} = gen_server:call('2', get),
	SynapseInfo = lists:keyfind('1', 1, Data),
	%Check if edge is added on the sender (the sensor)
	?assert(lists:member('2', Ids) /= false),
	%Check if edge is added on the receiver (the neuron)
	?assert(SynapseInfo /= false),
	{'1', sensor, Weight, Plasticity} = SynapseInfo, 
	%Check if the weight is of length 3
	?assert(length(Weight) == 3),
	%Check if the modulation  is hebbian of length 3
	?assert(element(1, Plasticity) == hebbian),
	?assert(length(element(2, Plasticity)) == 3),
	phenotype:delete(Phenotype).

add_synapses_from_model_test(_) ->
	Phenotype = get_default_phenotype(),
	phenotype:add_synapses(Phenotype, '1', '2', #{weight => [1,2,3], tag => {sensor, neuron}, modulation => none, connection_direction => forward}),
	#sensor_phenotype{output_elements_ids = Ids} = gen_server:call('1', get),
	#neuron_classic_phenotype{input_elements_data = Data} = gen_server:call('2', get),
	SynapseInfo = lists:keyfind('1', 1, Data),
	%Check if edge is added on the sender (the sensor)
	?assert(lists:member('2', Ids) /= false),
	%Check if edge is added on the receiver (the neuron)
	?assert(SynapseInfo /= false),
	{'1', sensor, Weight, _} = SynapseInfo, 
	%Check if the weight is of length 3
	?assert(length(Weight) == 3),
	phenotype:delete(Phenotype).

add_neuron_som_from_model_test(_) ->
	Phenotype = phenotype:new(som),
	phenotype:add_neuron(Phenotype, #{id => '1', weight => [1,2,3], coordinates => {0,0}, activation_function => manhattan}),
	%Check if the neuron is added
	?assert(whereis('1') /= undefined),
	%Check if the neuron som have a signal vector of length 4
	Label = gen_server:call('1', get),
	?assert(length(Label#neuron_som_phenotype.weight) == 3),
	phenotype:delete(Phenotype).

add_neuron_classic_from_model_test(_) ->
	Phenotype = phenotype:new(classic),
	phenotype:add_neuron(Phenotype, #{id => '1', bias =>1, layer => 1, activation_function => rectifier}),
	%Check if the neuron is added
	?assert(whereis('1') /= undefined),
	%Check if the neuron classic have the right label
	Label = gen_server:call('1', get),
	?assert(Label#neuron_classic_phenotype.layer == 1),
	phenotype:delete(Phenotype).

add_cortex_from_model_test(_) ->
	Phenotype = phenotype:new(classic),
	phenotype:add_cortex(Phenotype, #{id => '1', fit_directives => [], real_directives => [fun(X) -> X end]}),
	%Check if the cortex is added
	?assert(whereis('1') /= undefined),
	%Check if the cortex have the right label
	Label = gen_server:call('1', get),
	?assert(length(Label#cortex_phenotype.real_directives) == 1),
	phenotype:delete(Phenotype).

add_sensor_from_model_test(_) ->
	Phenotype = phenotype:new(classic),
	phenotype:add_sensor(Phenotype, #{id => '1', signal_input_length => 4, fit_directives => [fun(X) -> X end], real_directives => []}),
	%Check if the sensor is added
	?assert(whereis('1') /= undefined),
	%Check if the sensor have the right label
	Label = gen_server:call('1', get),
	?assert(length(Label#sensor_phenotype.fit_directives) == 1),
	phenotype:delete(Phenotype).

add_actuator_from_model_test(_) ->
	Phenotype = phenotype:new(classic),
	phenotype:add_actuator(Phenotype, #{id => '1', number_of_clients => 3, fit_directives => [], real_directives => []}),
	%Check if the sensor is added
	?assert(whereis('1') /= undefined),
	%Check if the actuator have the right label
	Label = gen_server:call('1', get),
	?assert(Label#actuator_phenotype.number_of_clients == 3),
	phenotype:delete(Phenotype).

update_element_test(_) ->
	Phenotype = get_default_phenotype(),
	%Set additional synapse for the update
	phenotype:add_synapses(Phenotype, '1', '2', #{signal_len => 1, tag => {sensor, neuron}, modulation_type => none, connection_direction => forward}),
	%Update neuron
	OldLabel = gen_server:call('2', get),
	phenotype:update_element(Phenotype, '2', {perturb_weights, 1, 100}),
	Neuron = gen_server:call('2', get),
	%Check if weights are perturbed
	?assert(Neuron#neuron_classic_phenotype.input_elements_data /= OldLabel#neuron_classic_phenotype.input_elements_data),
	phenotype:delete(Phenotype).

get_elements_filtered_test(_) ->
	Phenotype = get_default_phenotype(),
	Predicate = fun(X) -> is_record(X, cortex_phenotype) end,
	Collection = phenotype:get_elements_filtered(Phenotype, Predicate),
	%Check if the is the cortex and only the cortex
	?assert(length(Collection) == 1),
	[Cortex] = Collection,
	?assert(is_record(Cortex, cortex_phenotype)),
	phenotype:delete(Phenotype).

get_select_on_elements_filtered_test(_) ->
	Phenotype = get_default_phenotype(),
	Predicate = fun(X) -> is_record(X, neuron_classic_phenotype) orelse is_record(X, neuron_som_phenotype) end,
	SelectFunction = fun(X) -> 
						case X of
							#neuron_classic_phenotype{id = Id} -> Id;
							#neuron_som_phenotype{id = Id} -> Id
						end
					end,
	Collection = phenotype:get_select_on_elements_filtered(Phenotype, Predicate, SelectFunction),
	%Check if the is the neurons and only the neurons
	?assert(length(Collection) == 2),
	?assert(['2', '3'] == Collection),
	phenotype:delete(Phenotype).

get_element_by_id_test(_) ->
	Phenotype = get_default_phenotype(),
	Element = phenotype:get_element_by_id(Phenotype, '1'),
	%Check if sensor is taken
	?assert(is_record(Element, sensor_phenotype)),
	phenotype:delete(Phenotype).

get_sensors_test(_) ->
	Phenotype = get_default_phenotype(),
	Sensors = phenotype:get_sensors(Phenotype),
	%Check if sensor is taken
	?assert(length(Sensors) == 1),
	[Sensor] = Sensors,
	?assert(is_record(Sensor, sensor_phenotype)),
	phenotype:delete(Phenotype).

get_sensors_ids_test(_) ->
	Phenotype = get_default_phenotype(),
	SensorsIds = phenotype:get_sensors_ids(Phenotype),
	%Check if sensor is taken
	?assert(length(SensorsIds) == 1),
	?assert(['1'] == SensorsIds),
	phenotype:delete(Phenotype).

get_actuators_test(_) ->
	Phenotype = get_default_phenotype(),
	Actuators = phenotype:get_actuators(Phenotype),
	%Check if actuator is taken
	?assert(length(Actuators) == 1),
	[Actuator] = Actuators,
	?assert(is_record(Actuator, actuator_phenotype)),
	phenotype:delete(Phenotype).

get_actuators_ids_test(_) ->
	Phenotype = get_default_phenotype(),
	ActuatorsIds = phenotype:get_actuators_ids(Phenotype),
	%Check if actuator is taken
	?assert(length(ActuatorsIds) == 1),
	?assert(['4'] == ActuatorsIds),
	phenotype:delete(Phenotype).

get_neuron_ids_som_test(_) ->
	Phenotype = get_default_phenotype(),
	Ids = phenotype:get_neuron_ids(Phenotype),
	%Check if 1 neuron are taken, phenotype is classic!
	?assert(length(Ids) == 1),
	phenotype:delete(Phenotype).

get_neuron_ids_classic_test(_) ->
	Phenotype = get_default_phenotype(),
	Ids = phenotype:get_neuron_ids(Phenotype),
	%Check if the neuron classic was taken
	?assert(['2'] == Ids),
	phenotype:delete(Phenotype).

get_neurons_som_test(_) ->
	Phenotype = get_default_phenotype(),
	Collection = phenotype:get_neurons(Phenotype),
	%Check if 1 neuron are taken, phenotype is classic!
	?assert(length(Collection) == 1),
	phenotype:delete(Phenotype).

get_neurons_classic_test(_) ->
	Phenotype = get_default_phenotype(),
	Collection = phenotype:get_neurons(Phenotype),
	%Check if the neuron classic was taken
	?assert(length(Collection) == 1),
	phenotype:delete(Phenotype).

get_cortex_test(_) ->
	Phenotype = get_default_phenotype(),
	Cortex = phenotype:get_cortex(Phenotype),
	%Check if cortex is taken
	?assert(is_record(Cortex, cortex_phenotype)),
	phenotype:delete(Phenotype).

get_cortex_id_test(_) ->
	Phenotype = get_default_phenotype(),
	%Check if cortex is taken
	CortexId = phenotype:get_cortex_id(Phenotype),
	?assert(CortexId == '5'),
	phenotype:delete(Phenotype).

get_synapses_test(_) ->
	%The relative function must be implemented
	ok.

delete_element_test(_) ->
	Phenotype = get_default_phenotype(),
	phenotype:delete_element(Phenotype, '5'),
	%Check if the cortex there is no more
	?assert(whereis('5') == undefined),
	phenotype:delete(Phenotype).

delete_synapse_test(_) ->
	Phenotype = get_default_phenotype(),
	phenotype:delete_synapse(Phenotype, '2', '4'),
	#neuron_classic_phenotype{output_elements_ids = Out} = gen_server:call('2', get),
	#actuator_phenotype{input_elements_data = In} = gen_server:call('4', get),
	%Check if the synapse there is no more
	?assert(Out == []),
	?assert(length(In) == 0),
	phenotype:delete(Phenotype).
%%%

%%%Private function for this test module
get_default_phenotype() ->
	Phenotype = phenotype:new(classic),
	phenotype:add_element_with_phenotype(Phenotype, #sensor_phenotype{id = '1', signal_input_length = 3}),
	phenotype:add_element_with_phenotype(Phenotype, #neuron_classic_phenotype{id = '2', bias = 4}),
	phenotype:add_element_with_phenotype(Phenotype, #neuron_som_phenotype{id = '3', weight = 5}),
	phenotype:add_element_with_phenotype(Phenotype, #actuator_phenotype{id = '4', number_of_clients = 1}),
	phenotype:add_element_with_phenotype(Phenotype, #cortex_phenotype{id = '5'}),
	phenotype:add_synapses(Phenotype, '2', '4', #{signal_len => 1, tag => {neuron, actuator}, modulation_type => none, connection_direction => forward}),
	Phenotype.
%%%