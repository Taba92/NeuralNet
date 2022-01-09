-record(population,{
            id,
            agents=[]
        }).

-record(agent,{
            id,
            fitness = 0,
            scape_id,
            environment_path = ".", %string
            phenotype,
    }).

-record(phenotype,{
    network_type, %classic || som
    elements_dets % dets with object {Id, IsLocalOrRemote} 
    }).

-record(neuron_classic_phenotype,{
            id,
            layer,
            activation_function,
            bias,
            input_signals_data, %[{IdFrom, Weight, PlasticityData}]
            output_elements_ids, %[IdTo]
            recurrent_input_signals_data, %[{IdFrom, Weight, PlasticityData}]
            recurrent_output_elements_ids %[IdTo]
        }).

-record(neuron_som_phenotype,{
            id,
            coordinates,
            activation_function,
            weight,
            neighbors_ids,
            output_elements_ids,
            cluster
        }).

-record(sensor_phenotype,{
            id,
            signal_input_length,
            fit_directives,
            real_directives,
            output_elements_ids
        }).

-record(cortex_phenotype,{
            id,
            agent_id,
            fit_directives,
            real_directives,
            input_elements_ids, % typical actuators
            output_elements_ids, % typical sensors
        }).

-record(actuator_phenotype,{
            id,
            number_of_clients,
            fit_directives,
            real_directives,
            input_elements_ids, % typical neurons
            output_elements_ids, % typical cortex
        }).