-record(population,{
            id,
            agents=[]
        }).

-record(agent,{
            id,
            scape,
            cortex_id,
            genotype,
            fitness
    }).

-record(neuron_classic_phenotype,{
            id,
            layer,
            af,
            bias,
            faninsWeights,
            fanouts,
            roinsWeights,
            roouts
        }).

-record(neuron_som_phenotype,{
            id,
            coordinates,
            af,
            weight,
            neighbors,
            fanouts,
            cluster
        }).

-record(sensor_phenotype,{
            id,
            vl,
            fit_directives,
            real_directives,
            fanouts
        }).

-record(cortex_phenotype,{
            id,
            fit_directives,
            real_directives,
            sensorsIds,
            neuronsIds,
            actuatorsIds
        }).

-record(actuator_phenotype,{
            id,
            vl,
            fit_directives,
            real_directives,
            fanins,
            cortex_id
        }).