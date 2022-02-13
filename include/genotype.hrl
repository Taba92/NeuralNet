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
            cluster,
            activation_function
    }).

-record(cortex_genotype,{
    id,
    fit_directives,
    real_directives
    }).

-record(synapses,{
    id_from, 
    id_to, 
    weight,
    plasticity_modulation,
    tag, %{NodeTypeFrom, NodeTypeTo} where NodeType = cortex || sensor || neuron || actuator
    connection_direction % recurrent || forward 
    }).

-record(genotype,{
    network_type, % classic || som
    network %digraph
    }).