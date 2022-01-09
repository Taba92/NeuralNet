-record(sensor_genotype,{
    id,
    signal_input_length,
    fit_directives,
    real_directives
    }).

-record(actuator_genotype,{
    id,
    number_of_clients,
    fit_directives,
    real_directives
    }).

-record(neuron_classic_genotype,{
            id,
            layer, 
            bias,
            activation_function
            }).

-record(neuron_som_genotype,{
            id,
            coordinates, 
            weight,
            activation_function
    }).

-record(cortex_genotype,{
    id,
    fit_directives,
    real_directives
    }).

-record(synapses,{
    from,
    to,
    weight,
    plasticity_modulation,
    type % recurrent || forward 
    }).

-record(genotype,{
    network_type, % classic || som
    network, %digraph
    %%TODO LUCA: Campi da togliere, solo per compilare senza rotture
    neurons,
    sensors,
    actuators,
    cortex
    }).