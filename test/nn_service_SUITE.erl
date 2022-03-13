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

all() -> [cortex_genotype_to_phenotype_test, sensor_genotype_to_phenotype_test, actuator_genotype_to_phenotype_test,
        neuron_classic_genotype_to_phenotype_test, neuron_som_genotype_to_phenotype_test, synapse_genotype_to_phenotype_test,
        genotype_to_phenotype_test,
        cortex_phenotype_to_genotype_test, sensor_phenotype_to_genotype_test, actuator_phenotype_to_genotype_test,
        neuron_classic_phenotype_to_genotype_test, neuron_som_phenotype_to_genotype_test,
        synapse_to_cortex_phenotype_to_genotype_test, synapse_to_sensor_phenotype_to_genotype_test,
        synapse_to_neuron_classic_phenotype_to_genotype_test, synapse_to_neuron_som_phenotype_to_genotype_test, synapse_to_actuator_phenotype_to_genotype_test,
        phenotype_to_genotype_test].

cortex_genotype_to_phenotype_test(_) ->
    %nn_service:element_genotype_to_phenotype(Phenotype, ElementGenotype) when is_record(ElementGenotype, cortex_genotype),
    ok.
sensor_genotype_to_phenotype_test(_) ->
    %nn_service:element_genotype_to_phenotype(Phenotype, ElementGenotype) when is_record(ElementGenotype, sensor_genotype),
    ok.
actuator_genotype_to_phenotype_test(_) ->
    %nn_service:element_genotype_to_phenotype(Phenotype, ElementGenotype) when is_record(ElementGenotype, actuator_genotype),
    ok.
neuron_classic_genotype_to_phenotype_test(_) ->
    %nn_service:element_genotype_to_phenotype(Phenotype, ElementGenotype) when is_record(ElementGenotype, neuron_classic_genotype),
    ok.
neuron_som_genotype_to_phenotype_test(_) ->
    %nn_service:element_genotype_to_phenotype(Phenotype, ElementGenotype) when is_record(ElementGenotype, neuron_som_genotype),
    ok.
synapse_genotype_to_phenotype_test(_) ->
    %nn_service:element_genotype_to_phenotype(Phenotype, SynapseGenotype) when is_record(ElementGenotype, synapses),
    ok.

genotype_to_phenotype_test(_) ->
    %nn:service:genotype_to_phenotype(Agent, Genotype)
    ok.

cortex_phenotype_to_genotype_test(_) ->
    %nn_service:element_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, cortex_phenotype),
    ok.
sensor_phenotype_to_genotype_test(_) ->
    %nn_service:element_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, sensor_phenotype),
    ok.
actuator_phenotype_to_genotype_test(_) ->
    %nn_service:element_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, actuator_phenotype),
    ok.
neuron_classic_phenotype_to_genotype_test(_) ->
    %nn_service:element_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, neuron_classic_phenotype),
    ok.
neuron_som_phenotype_to_genotype_test(_) ->
    %nn_service:element_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, neuron_som_phenotype),
    ok.

synapse_to_cortex_phenotype_to_genotype_test(_) ->
    %nn_service:synapse_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, cortex_phenotype),
    ok.
synapse_to_sensor_phenotype_to_genotype_test(_) ->
    %nn_service:synapse_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, sensor_phenotype),
    ok.
synapse_to_neuron_classic_phenotype_to_genotype_test(_) ->
    %nn_service:synapse_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, neuron_classic_phenotype),
    ok.
synapse_to_neuron_som_phenotype_to_genotype_test(_) ->
    %nn_service:synapse_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, neuron_som_phenotype),
    ok.
synapse_to_actuator_phenotype_to_genotype_test(_) ->
    %nn_service:synapse_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, actuator_phenotype),
    ok.

phenotype_to_genotype_test(_) ->
    %nn_service:phenotype_to_genotype(Agent, Phenotype),
    ok.