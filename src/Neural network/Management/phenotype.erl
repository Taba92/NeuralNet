-module(phenotype).
-export([geno_to_pheno/1,pheno_to_geno/1]).
-export([stop_phenotype/1,backup_weights/1,perturb_weights/1,restore_weights/1,cluster_setting/2]).
-export([link_to_cortex/2,link_nn_to_scape/2]).
-export([get_select_on_elements_filtered/3, get_elements_filtered/2, get_element_by_id/2]).
-export([get_sensors/1, get_sensors_ids/1, get_actuators/1, get_actuators_ids/1, get_neurons/1, get_neuron_ids/1, get_cortex/1, get_cortex_id/1, get_synapses/3]).
-export([update_element/3]).
-export([add_neuron/2, add_actuator/2, add_sensor/2, add_cortex/2, add_synapses/4]).
-export([delete_element/2, delete_synapse/3]).
-include("utils.hrl").
-include("phenotype.hrl").

stop_phenotype(CortexId)->
	CortexGeno=gen_server:call(CortexId,dump,infinity),
	#cortex_phenotype{sensorsIds=SensorsIds,neuronsIds=NeuronsIds,actuatorsIds=ActuatorsIds}=CortexGeno,
	[gen_server:stop(Pid,normal,infinity)||Pid<-[CortexId]++SensorsIds++NeuronsIds++ActuatorsIds],
	ok.

backup_weights(CortexId)->
	CortexGeno=gen_server:call(CortexId,dump,infinity),
	#cortex_phenotype{neuronsIds=NeuronsIds}=CortexGeno,
	[gen_server:call(Pid,backup_weights,infinity)||Pid<-NeuronsIds],
	ok.

perturb_weights({CortexId,Prob,StepW})->
	CortexGeno=gen_server:call(CortexId,dump,infinity),
	#cortex_phenotype{neuronsIds=NeuronsIds}=CortexGeno,
	[gen_server:call(Pid,{perturb_weights,Prob,StepW},infinity)||Pid<-NeuronsIds],
	ok.

restore_weights(CortexId)->
	CortexGeno=gen_server:call(CortexId,dump,infinity),
	#cortex_phenotype{neuronsIds=NeuronsIds}=CortexGeno,
	[gen_server:call(Pid,restore_weights,infinity)||Pid<-NeuronsIds],
	ok.

cluster_setting(CortexId,Centroids)->
	CortexGeno=gen_server:call(CortexId,dump,infinity),
	#cortex_phenotype{neuronsIds=NeuronsIds}=CortexGeno,
	[gen_server:call(Pid,{cluster_setting,Centroids},infinity)||Pid<-NeuronsIds],
	ok.

%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_elements_filtered(Phenotype, Predicate) ->
	Dets = Phenotype#phenotype.elements_dets,
	% In the phenotype table elements data are stored as a tuple {Id, IsRemoteOrLocal}
	Filter = fun({ElementId, _}, Acc) ->
				ElementPhenotype = get_element_by_id(ElementId),
				case Predicate(ElementPhenotype) of
					true -> [El | Acc];
					false -> Acc
				end
			end,
	dets:foldl(Filter, [], Dets).

get_select_on_elements_filtered(Phenotype, Predicate, SelectFunction) ->
	Dets = Phenotype#phenotype.elements_dets,
	% In the phenotype table elements data are stored as a tuple {Id, IsRemoteOrLocal}
	Filter = fun({ElementId, _}, Acc) ->
				ElementPhenotype = get_element_by_id(ElementId),
				case Predicate(ElementPhenotype) of
					true -> [SelectFunction(ElementPhenotype) | Acc];
					false -> Acc
				end
			end,
	dets:foldl(Filter, [], Dets).

get_element_by_id(_, ElementId) ->
	ElementPhenotype = gen_server:call(ElementId, get, infinity),
	ElementPhenotype.

get_sensors(Phenotype) ->
	Predicate = fun(Element) -> is_record(Element, sensor_phenotype) end,
	get_elements_filtered(Phenotype, Predicate).

get_sensors_ids(Phenotype) ->
	Predicate = fun(Element) -> is_record(Element, sensor_phenotype) end,
	Select = fun(#sensor_phenotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Phenotype, Predicate, Select).

get_actuators(Phenotype) ->
	Predicate = fun(El) -> is_record(El, actuator_phenotype) end,
	get_elements_filtered(Phenotype, Predicate).

get_actuators_ids(Phenotype) ->
	Predicate = fun(El) -> is_record(El, actuator_phenotype) end,
	Select = fun(#actuator_phenotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Phenotype, Predicate, Select).

get_cortex(Phenotype) ->
	Predicate = fun(El) -> is_record(El, cortex_phenotype) end,
	[Cortex] = get_elements_filtered(Phenotype, Predicate),
	Cortex.

get_cortex_id(Phenotype) ->
	Cortex = get_cortex(Phenotype),
	#cortex_phenotype{id = Id} = Cortex,
	Id.

get_neuron_ids(Phenotype) when Phenotype#phenotype.network_type == som ->
	Predicate = fun(El) -> is_record(El, neuron_som_phenotype) end,
	Select = fun(#neuron_som_phenotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Phenotype, Predicate, Select);
get_neuron_ids(Phenotype) when Phenotype#phenotype.network_type == classic->
	Predicate = fun(El) -> is_record(El, neuron_classic_phenotype) end,
	Select = fun(#neuron_classic_phenotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Phenotype, Predicate, Select).

get_neurons(Phenotype) when Phenotype#phenotype.network_type == som->
	Predicate = fun(El) -> is_record(El, neuron_som_phenotype) end,
	get_elements_filtered(Phenotype, Predicate);
get_neurons(Phenotype) when Phenotype#phenotype.network_type == classic->
	Predicate = fun(El) -> is_record(El, neuron_classic_phenotype) end,
	get_elements_filtered(Genotype, Predicate).

get_synapses(Phenotype, IdFrom, IdTo) ->
	%The node receiver hold the informations about synapse
	throw(to_be_implemented).


%Create element using existing model, derived from own genotype in base of model type
add_cortex(Phenotype, #{id := Id, fit_directives := Fit, real_directives := Real}) ->
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:insert(Dets, {Id, local}),
	CortexPhenotype = #cortex_phenotype{id = CortexId, fit_directives = FitDirectives, real_directives = RealDirectives},
	gen_server:start({local, Id}, ?CORTEX_MODULE, [CortexPhenotype], []),
	CortexId.

add_sensor(Phenotype, #{id := Id, signal_input_length := InputLength, fit_directives := Fit, real_directives := Real}) ->
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:insert(Dets, {Id, local}),
	SensorPhenotype = #sensor_phenotype{id = Id, signal_input_length = InputLength, fit_directives = FitDirectives, real_directives = RealDirectives},
	gen_server:start({local, Id}, ?SENSOR_MODULE, [SensorPhenotype], []),
	SensorId.

add_actuator(Phenotype, #{id := Id, number_of_clients := NumClients, fit_directives := Fit, real_directives := Real}) ->
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:insert(Dets, {Id, local}),
	ActuatorPhenotype = #actuator_phenotype{id = ActuatorId, number_of_clients = NumClients, fit_directives = FitDirectives, real_directives = RealDirectives},
	gen_server:start({local, Id}, ?ACTUATOR_MODULE, [ActuatorPhenotype], []),
	ActuatorId.

add_neuron(Phenotype, #{id := Id, coordinates := Coord, weight := Weight, activation_function := ActivationFunction}) when Phenotype#phenotype.network_type == som ->
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:insert(Dets, {Id, local}),
	NeuronSomPhenotype = #neuron_som_phenotype{id = NeuronId, weight = Weight, coordinates = Coordinates, activation_function = ActivationFunction},
	gen_server:start({local, Id}, ?SOM_PHENOTYPE_MODULE, [NeuronSomPhenotype], []),
	NeuronId;
add_neuron(Phenotype, #{id := Id, layer := Layer, bias := Bias, activation_function := ActivationFunction}) when Phenotype#phenotype.network_type == classic ->
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:insert(Dets, {Id, local}),
	NeuronClassicPhenotype = #neuron_classic_phenotype{id = NeuronId, bias = Bias, layer = Layer, activation_function = ActivationFunction},
	gen_server:start({local, Id}, ?CLASSIC_PHENOTYPE_MODULE, [NeuronClassicPhenotype], []),
	NeuronId.

%Inizialize synapses with new parameters
add_synapses(Phenotype, IdFrom, IdTo, #{signal_len := SignalLength, tag := Tag, modulation_type := NeuroModulationType, connection_direction := ConnectionDirection}) ->
	Weight = utils:get_random_list(SignalLength),
	Modulation = plasticity:get_plasticity(Weight, NeuroModulationType),
	SynapseLabel = #{weight => Weight, tag => Tag, modulation => Modulation, connection_direction => ConnectionDirection},
	add_synapses(Phenotype, IdFrom, IdTo, SynapseLabel);
%Initialize synapses with parameters already present
add_synapses(Phenotype, IdFrom, IdTo, #{weight := Weight, tag := Tag, modulation := Modulation, connection_direction := ConnectionDirection}) ->
	%Create the phenotype of the sinapses
	SinapsesPhenotype = {IdFrom, IdTo, Tag, Weight, Modulation, ConnectionDirection},
	%The emanate node will store only the id of the receiver (IdTo)
	update_element(Phenotype, IdFrom, {add_synapses, SinapsesPhenotype}),
	% The incident node will store the information for process inbound signals from IdFrom
	update_element(Phenotype, IdTo, {add_synapses, SinapsesPhenotype}),
	EdgeId;


%Send directive to the element with id = ElementId
update_element(_, ElementId, DirectiveLabel) ->
	gen_server:call(ElementId, {update, DirectiveLabel}, infinity),
	ok.

delete_element(Phenotype, ElementId) ->
	%NB: This method delete ONLY the node. Every inbound and outbound sinapses of this node are maintained on others the connected nodes!
	#phenotype{elements_dets = Dets} = Phenotype,
	dets:delete(Dets, ElementId),
	gen_server:stop(ElementId, normal, infinity),
	ok.

delete_synapse(Phenotype, IdFrom, IdTo) ->
	update_element(Phenotype, IdFrom, {delete_synapses, IdFrom, IdTo}),
	update_element(Phenotype, IdTo, {delete_synapses, IdFrom, IdTo}),
	ok.
