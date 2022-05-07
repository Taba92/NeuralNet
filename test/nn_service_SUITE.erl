-module(nn_service_SUITE).
-export([all/0]).
-export([cortex_genotype_to_phenotype_test/1, sensor_genotype_to_phenotype_test/1, actuator_genotype_to_phenotype_test/1,
        neuron_classic_genotype_to_phenotype_test/1, neuron_som_genotype_to_phenotype_test/1, synapse_genotype_to_phenotype_test/1,
        genotype_to_phenotype_test/1,
        cortex_phenotype_to_genotype_test/1, sensor_phenotype_to_genotype_test/1, actuator_phenotype_to_genotype_test/1,
        neuron_classic_phenotype_to_genotype_test/1, neuron_som_phenotype_to_genotype_test/1,
        synapse_to_cortex_phenotype_to_genotype_test/1, synapse_to_sensor_phenotype_to_genotype_test/1,
        synapse_to_neuron_classic_phenotype_to_genotype_test/1, synapse_to_neuron_som_phenotype_to_genotype_test/1, synapse_to_actuator_phenotype_to_genotype_test/1,
        phenotype_to_genotype_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("phenotype.hrl").
-include("genotype.hrl").

all() -> [cortex_genotype_to_phenotype_test, sensor_genotype_to_phenotype_test, actuator_genotype_to_phenotype_test,
        neuron_classic_genotype_to_phenotype_test, neuron_som_genotype_to_phenotype_test, synapse_genotype_to_phenotype_test,
        genotype_to_phenotype_test,
        cortex_phenotype_to_genotype_test, sensor_phenotype_to_genotype_test, actuator_phenotype_to_genotype_test,
        neuron_classic_phenotype_to_genotype_test, neuron_som_phenotype_to_genotype_test,
        synapse_to_cortex_phenotype_to_genotype_test, synapse_to_sensor_phenotype_to_genotype_test,
        synapse_to_neuron_classic_phenotype_to_genotype_test, synapse_to_neuron_som_phenotype_to_genotype_test, synapse_to_actuator_phenotype_to_genotype_test,
        phenotype_to_genotype_test].

cortex_genotype_to_phenotype_test(_) ->
    Phenotype = phenotype:new(classic),
    ElementGenotype = #cortex_genotype{id = '1', fit_directives = [], real_directives = []},
    nn_service:element_genotype_to_phenotype(Phenotype, ElementGenotype),
    %Check if the phenotype contains the cortex
    CortexId = phenotype:get_cortex_id(Phenotype),
    ?assert('1' == CortexId),
    phenotype:delete(Phenotype).

sensor_genotype_to_phenotype_test(_) ->
    Phenotype = phenotype:new(classic),
    ElementGenotype = #sensor_genotype{id = '2', signal_input_length = 3, fit_directives = [], real_directives = []},
    nn_service:element_genotype_to_phenotype(Phenotype, ElementGenotype),
    %Check if the phenotype contains the sensor
    SensorsIds = phenotype:get_sensors_ids(Phenotype),
    ?assert(lists:member('2', SensorsIds)),
    phenotype:delete(Phenotype).

actuator_genotype_to_phenotype_test(_) ->
    Phenotype = phenotype:new(classic),
    ElementGenotype = #actuator_genotype{id = '2', number_of_clients = 3, fit_directives = [], real_directives = []},
    nn_service:element_genotype_to_phenotype(Phenotype, ElementGenotype),
    %Check if the phenotype contains the actuator
    ActuatorsIds = phenotype:get_actuators_ids(Phenotype),
    ?assert(lists:member('2', ActuatorsIds)),
    phenotype:delete(Phenotype).

neuron_classic_genotype_to_phenotype_test(_) ->
    Phenotype = phenotype:new(classic),
    ElementGenotype = #neuron_classic_genotype{id = '2', layer = 3, bias = 1, activation_function = rectifier},
    nn_service:element_genotype_to_phenotype(Phenotype, ElementGenotype),
    %Check if the phenotype contains the neuron
    NeuronsIds = phenotype:get_neuron_ids(Phenotype),
    ?assert(lists:member('2', NeuronsIds)),
    phenotype:delete(Phenotype).

neuron_som_genotype_to_phenotype_test(_) ->
    Phenotype = phenotype:new(som),
    ElementGenotype = #neuron_som_genotype{id = '2'},
    nn_service:element_genotype_to_phenotype(Phenotype, ElementGenotype),
    %Check if the phenotype contains the neuron
    NeuronsIds = phenotype:get_neuron_ids(Phenotype),
    ?assert(lists:member('2', NeuronsIds)),
    phenotype:delete(Phenotype).

synapse_genotype_to_phenotype_test(_) ->
    Phenotype = get_default_phenotype(),
    ElementGenotype = #synapses{id_from = '5', id_to = '1', tag = {cortex, sensor}, connection_direction = forward},
    nn_service:synapse_genotype_to_phenotype(Phenotype, ElementGenotype),
    %Check if the phenotype contains the synapse
    Cortex = phenotype:get_cortex(Phenotype),
    Sensor = phenotype:get_element_by_id(Phenotype, '1'),
    ?assert(Sensor#sensor_phenotype.input_elements_data == [{'5', cortex}]),
    ?assert(Cortex#cortex_phenotype.output_elements_ids == ['1']),
    phenotype:delete(Phenotype).

genotype_to_phenotype_test(_) ->
    Agent = #agent{id = luca},
    Genotype = get_default_genotype(),
    #agent{phenotype = Phenotype} = nn_service:genotype_to_phenotype(Agent, Genotype),
    %Prepare filter for get all neurons
    Predicate = fun(X) -> is_record(X, neuron_classic_phenotype) orelse is_record(X, neuron_som_phenotype) end,
	SelectFunction = fun(X) -> 
						case X of
							#neuron_classic_phenotype{id = Id} -> Id;
							#neuron_som_phenotype{id = Id} -> Id
						end
					end,
    %Check the resultant phenotype
    SensorsIds = phenotype:get_sensors_ids(Phenotype),
    ActuatorsIds = phenotype:get_actuators_ids(Phenotype),
    NeuronsIds = phenotype:get_select_on_elements_filtered(Phenotype, Predicate, SelectFunction),
    ?assert(SensorsIds == ['1']),
    ?assert(ActuatorsIds == ['4']),
    ?assert(NeuronsIds == ['2']),
    phenotype:delete(Phenotype).

cortex_phenotype_to_genotype_test(_) ->
    Genotype = genotype:new(classic),
    ElementPhenotype = #cortex_phenotype{id = '1'},
    nn_service:element_phenotype_to_genotype(Genotype, ElementPhenotype),
    %Check if the genotype contains the cortex
    CortexId = genotype:get_cortex_id(Genotype),
    ?assert(CortexId == '1'). 

sensor_phenotype_to_genotype_test(_) ->
    Genotype = genotype:new(classic),
    ElementPhenotype = #sensor_phenotype{id = '1'},
    nn_service:element_phenotype_to_genotype(Genotype, ElementPhenotype),
    %Check if the genotype contains the sensor
    SensorsIds = genotype:get_sensors_ids(Genotype),
    ?assert(SensorsIds == ['1']).

actuator_phenotype_to_genotype_test(_) ->
    Genotype = genotype:new(classic),
    ElementPhenotype = #actuator_phenotype{id = '1'},
    nn_service:element_phenotype_to_genotype(Genotype, ElementPhenotype),
    %Check if the genotype contains the sensor
    ActuatorsIds = genotype:get_actuators_ids(Genotype),
    ?assert(ActuatorsIds == ['1']).

neuron_classic_phenotype_to_genotype_test(_) ->
    Genotype = genotype:new(classic),
    ElementPhenotype = #neuron_classic_phenotype{id = '1'},
    nn_service:element_phenotype_to_genotype(Genotype, ElementPhenotype),
    %Check if the genotype contains the neuron
    NeuronsIds = genotype:get_neuron_ids(Genotype),
    ?assert(NeuronsIds == ['1']).

neuron_som_phenotype_to_genotype_test(_) ->
    Genotype = genotype:new(som),
    ElementPhenotype = #neuron_som_phenotype{id = '1'},
    nn_service:element_phenotype_to_genotype(Genotype, ElementPhenotype),
    %Check if the genotype contains the neuron
    NeuronsIds = genotype:get_neuron_ids(Genotype),
    ?assert(NeuronsIds == ['1']).

synapse_to_cortex_phenotype_to_genotype_test(_) ->
    Genotype = get_default_genotype(),
    ElementPhenotype = #cortex_phenotype{id = '5', input_elements_data = [{'4', actuator}]},
    nn_service:synapse_phenotype_to_genotype(Genotype, ElementPhenotype),
    %Check if the synapse is added
    Synapse = genotype:get_synapses(Genotype, '4', '5'),
    ?assert(is_record(Synapse, synapses)),
    #synapses{id_from = IdFrom, id_to = IdTo} = Synapse,
    ?assert((IdFrom == '4' andalso  IdTo == '5')).

synapse_to_sensor_phenotype_to_genotype_test(_) ->
    Genotype = get_default_genotype(),
    ElementPhenotype = #sensor_phenotype{id = '1', input_elements_data = [{'5', cortex}]},
    nn_service:synapse_phenotype_to_genotype(Genotype, ElementPhenotype),
    %Check if the synapse is added
    Synapse = genotype:get_synapses(Genotype, '5', '1'),
    ?assert(is_record(Synapse, synapses)),
    #synapses{id_from = IdFrom, id_to = IdTo} = Synapse,
    ?assert((IdFrom == '5' andalso  IdTo == '1')).

synapse_to_neuron_classic_phenotype_to_genotype_test(_) ->
    Genotype = get_default_genotype(),
    ElementPhenotype = #neuron_classic_phenotype{id = '2', input_elements_data = [{'1', sensor, 0, none}]},
    nn_service:synapse_phenotype_to_genotype(Genotype, ElementPhenotype),
    %Check if the synapse is added
    Synapse = genotype:get_synapses(Genotype, '1', '2'),
    ?assert(is_record(Synapse, synapses)),
    #synapses{id_from = IdFrom, id_to = IdTo} = Synapse,
    ?assert((IdFrom == '1' andalso  IdTo == '2')).

synapse_to_neuron_som_phenotype_to_genotype_test(_) ->
    Genotype = get_default_genotype(),
    ElementPhenotype = #neuron_som_phenotype{id = '3', input_elements_data = [{'1', sensor}]},
    nn_service:synapse_phenotype_to_genotype(Genotype, ElementPhenotype),
    %Check if the synapse is NOT added, there isn't the neuron som in the default genotype
    Synapse = genotype:get_synapses(Genotype, '1', '3'),
    ?assert(Synapse == false).

synapse_to_actuator_phenotype_to_genotype_test(_) ->
    Genotype = get_default_genotype(),
    ElementPhenotype = #actuator_phenotype{id = '4', input_elements_data = [{'1', sensor}]},
    nn_service:synapse_phenotype_to_genotype(Genotype, ElementPhenotype), 
    %Check if the synapse is added
    Synapse = genotype:get_synapses(Genotype, '1', '4'),
    ?assert(is_record(Synapse, synapses)),
    #synapses{id_from = IdFrom, id_to = IdTo} = Synapse,
    ?assert((IdFrom == '1' andalso  IdTo == '4')).

phenotype_to_genotype_test(_) ->
    Agent = #agent{id = luca},
    Phenotype = get_default_phenotype(),
    {_, Genotype} = nn_service:phenotype_to_genotype(Agent, Phenotype),
    %Prepare filter for get all neurons
    Predicate = fun(X) -> is_record(X, neuron_classic_genotype) orelse is_record(X, neuron_som_genotype) end,
	SelectFunction = fun(X) -> 
						case X of
							#neuron_classic_genotype{id = Id} -> Id;
							#neuron_som_genotype{id = Id} -> Id
						end
					end,
    %Check the resultant phenotype
    SensorsIds = genotype:get_sensors_ids(Genotype),
    ActuatorsIds = genotype:get_actuators_ids(Genotype),
    NeuronsIds = genotype:get_select_on_elements_filtered(Genotype, Predicate, SelectFunction),
    ?assert(SensorsIds == ['1']),
    ?assert(ActuatorsIds == ['4']),
    ?assert(NeuronsIds == ['2']).


%%%Private function for this test module
get_default_phenotype() ->
	Phenotype = phenotype:new(classic),
	phenotype:add_element_with_phenotype(Phenotype, #sensor_phenotype{id = '1', signal_input_length = 3}),
	phenotype:add_element_with_phenotype(Phenotype, #neuron_classic_phenotype{id = '2', bias = 4}),
	phenotype:add_element_with_phenotype(Phenotype, #actuator_phenotype{id = '4', number_of_clients = 1}),
	phenotype:add_element_with_phenotype(Phenotype, #cortex_phenotype{id = '5'}),
	phenotype:add_synapses(Phenotype, '2', '4', #{signal_len => 1, tag => {neuron, actuator}, modulation_type => none, connection_direction => forward}),
	Phenotype.

get_default_genotype() ->
	Genotype = genotype:new(classic),
	genotype:add_element_with_genotype(Genotype, #sensor_genotype{id = '1', signal_input_length = 3}),
	genotype:add_element_with_genotype(Genotype, #neuron_classic_genotype{id = '2', bias = '4'}),
	genotype:add_element_with_genotype(Genotype, #actuator_genotype{id = '4', number_of_clients = 1}),
	genotype:add_element_with_genotype(Genotype, #cortex_genotype{id = '5'}),
	genotype:add_element_with_genotype(Genotype, #synapses{id_from = '2', id_to = '4', tag = {neuron,actuator}, plasticity_modulation = none, connection_direction = forward}),
	Genotype.
%%%