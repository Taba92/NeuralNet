-record(population,{
            id,
            agents=[]
        }).

-record(agent,{
            id,
            fitness = 0,
            scape_id,
            environment_path = ".", %string
            phenotype
    }).

-record(phenotype,{
    network_type, %classic || som
    elements_dets % dets with object {Id, NodeType, IsLocalOrRemote} 
    }).

-record(neuron_classic_phenotype,{
            id,
            layer,
            activation_function,
            bias,
            input_elements_data, %[{IdFrom, NodeType, Weight, PlasticityData}] NodeType typical is sensor or neuron
            output_elements_ids, %[IdTo]
            recurrent_input_elements_data, %[{IdFrom, NodeType, Weight, PlasticityData}] NodeType typical is neuron
            recurrent_output_elements_ids %[IdTo]
        }).

-record(neuron_som_phenotype,{
            id,
            coordinates,
            activation_function,
            weight,
            cluster,
            neighbors_data, %[{IdFrom, Type}] where Type = forward || recurrent
            input_elements_data, %[{IdFrom, NodeType}] NodeType typical is sensor
            output_elements_ids % [IdTo] typical actuators ids
        }).

-record(sensor_phenotype,{
            id,
            signal_input_length,
            fit_directives,
            real_directives,
            input_elements_data, %[{IdFrom, NodeType}] NodeType typical is cortex
            output_elements_ids % typical neurons ids
        }).

-record(cortex_phenotype,{
            id,
            agent_id,
            fit_directives,
            real_directives,
            input_elements_data, %[{IdFrom, NodeType}] NodeType typical is actuator
            output_elements_ids % typical sensors ids
        }).

-record(actuator_phenotype,{
            id,
            number_of_clients,
            fit_directives,
            real_directives,
            input_elements_data, %[{IdFrom, NodeType}] NodeType typical is neuron
            output_elements_ids % typical cortex
        }).