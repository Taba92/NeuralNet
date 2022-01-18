-module(nn_service).
-export([genotype_to_phenotype/1, phenotype_to_genotype/2]).
-export([apply_directives_pipe/2, order_by_keylist/2, apply_to_scape/2, perturbate/1]).
-include("utils.hrl").
-include("genotype.hrl").
-include("phenotype.hrl").

get_neuron_phenotype_module(normal) -> neuron;
get_neuron_phenotype_module(som) -> neuron_som.

%%%Start mapping section
%%%
%Retrive the phenotype from the genotype, starting every node and synapse, erasing the last one.
genotype_to_phenotype(Agent, Genotype) when is_record(Agent, agent),is_record(Genotype, genotype) ->
	#genotype{network = Network, network_type = NetworkType} = Genotype,
	#agent{id = AgentId, environment_path = Env} = Agent,
	AgentDirectoryPath = Env ++ atom_to_list(AgentId) ++ "/",
	%1) Create the directory environment of the agent if not exist
	filelib:ensure_dir(AgentDirectoryPath) 
	%2) Create dets file in the directory environment of the agent
	{ok, PhenotypeDets} = dets:open(AgentId, [{file, AgentDirectoryPath ++ atom_to_list(AgentId)}]),
	%3) Create the phenotype data structure
	Phenotype = #phenotype{network_type = NetworkType, elements_dets = PhenotypeDets},
	%4) Map nodes and synapses to its phenotype
	%4.1) Prepare structures for mapping
	{digraph, Vertices, Edges, _} = Network,
	%4.1.1) Hold vertices and edges in dets temporary files, to avoid duplication of data in the memory
	{ok, VerticesTemp} = dets:open(vertices_temp, [{file, AgentDirectoryPath ++ "vertices_temp"}]),
	ets:to_dets(Vertices, VerticesTemp),
	{ok, EdgesTemp} = dets:open(edges_temp, [{file, AgentDirectoryPath ++ "edges_temp"}]),
	ets:to_dets(Edges, EdgesTemp),
	%4.1.2) Now i can remove the genotype from memory
	digraph:delete(Network),
	%4.2) Start the true mapping
	%4.2.1) Launch all nodes
	NodesMappingFun = fun({ElementId, ElementGenotype}) ->
							element_genotype_to_phenotype(Phenotype, ElementGenotype),
							continue,
						end,
	dets:traverse(VerticesTemp, NodesMappingFun),
	%4.2.1) Connect all nodes, launching all synapses
	EdgesMappingFun = fun({SynapseId, SynapseGenotype}) ->
							synapse_genotype_to_phenotype(Phenotype, SynapseGenotype),
							continue
						end,
	dets:traverse(EdgesTemp, EdgesMappingFun),
	%5) Delete temporary files
	file:delete(AgentDirectoryPath ++ "vertices_temp"),
	file:delete(AgentDirectoryPath ++ "edges_temp")
	%6) Connect the cortex with the agent
	CortexId = phenotype:get_cortex_id(Phenotype),
	phenotype:update_element(CortexId, {set_agent_id, AgentId}),
	Agent{phenotype = Phenotype}.
	
element_genotype_to_phenotype(Phenotype, ElementGenotype) when is_record(ElementGenotype, cortex_genotype) ->
	#cortex_genotype{id = Id, fit_directives = FitDirectives, real_directives = RealDirectives} = ElementGenotype,
	CortexPhenotypeLabel = #{id => Id, fit_directives => FitDirectives, real_directives => RealDirectives},
	phenotype:add_cortex(Phenotype, CortexPhenotypeLabel);
element_genotype_to_phenotype(Phenotype, ElementGenotype) when is_record(ElementGenotype, sensor_genotype) ->
	#sensor_genotype{id = Id, signal_input_length = SignalInputLength, fit_directives = FitDirectives, real_directives = RealDirectives} = ElementGenotype,
	SensorPhenotypeLabel = #{id => Id, signal_input_length => SignalInputLength, fit_directives => FitDirectives, real_directives => RealDirectives},
	phenotype:add_cortex(Phenotype, SensorPhenotypeLabel);
element_genotype_to_phenotype(Phenotype, ElementGenotype) when is_record(ElementGenotype, neuron_classic_genotype) ->
	#neuron_classic_genotype{id = Id, layer = Layer, bias = Bias, activation_function = ActivationFunction} = ElementGenotype,
	NeuronClassicPhenotypeLabel = #{id => Id, layer => Layer, bias => Bias, activation_function => ActivationFunction},
	phenotype:add_cortex(Phenotype, NeuronClassicPhenotypeLabel);
element_genotype_to_phenotype(Phenotype, ElementGenotype) when is_record(ElementGenotype, neuron_som_genotype) ->
	#neuron_som_genotype{id = Id, coordinates = Coordinates, weight = Weight, activation_function = ActivationFunction} = ElementGenotype,
	NeuronSomPhenotypeLabel = #{id => Id, coordinates => Coordinates, weight => Weight, activation_function => ActivationFunction},
	phenotype:add_cortex(Phenotype, NeuronSomPhenotypeLabel);
element_genotype_to_phenotype(Phenotype, ElementGenotype) when is_record(ElementGenotype, actuator_genotype) ->
	#actuator_genotype{id = Id, number_of_clients = NumberOfClients, fit_directives = FitDirectives, real_directives = RealDirectives} = ElementGenotype,
	ActuatorPhenotypeLabel = #{id => Id, number_of_clients => NumberOfClients, fit_directives => FitDirectives, real_directives => RealDirectives},
	phenotype:add_cortex(Phenotype, ActuatorPhenotypeLabel).
synapse_genotype_to_phenotype(Phenotype, SynapseGenotype) when is_record(SynapseGenotype, synapses) ->
	#synapses{id_from = From, id_to = To, tag = Tag, weight = Weight, plasticity_modulation = Modulation, type = Type} = SynapseGenotype,
	SynapsePhenotypeLabel = #{weight => Weight, tag => Tag, modulation => Modulation, connection_direction => Type},
	phenotype:add_synapses(Phenotype, From, To, SynapsePhenotypeLabel).

%Recover the genotype from the phenotype, stopping the last one.
phenotype_to_genotype(Agent, Phenotype) when is_record(Agent, agent),is_record(Phenotype, phenotype) ->
	#phenotype{network_type = NetType, elements_dets = ElementsIdsDets} = Phenotype,
	#agent{id = AgentId, environment_path = Env} = Agent,
	AgentDirectoryPath = Env ++ atom_to_list(AgentId) ++ "/",
	%1) Create the directory environment of the agent if not exist
	filelib:ensure_dir(AgentDirectoryPath) 
	%2) Initialize the new genotype
	Genotype = #genotype{network_type = Type, network = digraph:new()},
	%3) Create the temporary file for store all elements phenotypes
	{ok, PhenotypesTemp} = dets:open(phenotypes_temp, [{file, AgentDirectoryPath ++ "phenotypes_temp"}]),
	%4) Map nodes and synapses to its genotype
	%4.1) Collect all elements phenotype, to avoid memory duplication
	CollectPhenotypesFun = fun({ElementId, _}) ->
					ElementPhenotype = phenotype:get_element_by_id(Phenotype, ElementId),
					dets:insert(PhenotypesTemp, {ElementId, ElementPhenotype}),
					phenotype:delete_element(Phenotype, ElementId),
					continue
				end,
	dets:traverse(ElementsIdsDets, CollectPhenotypesFun),
	%4.2) Start the true mapping. Remember that the informations about synapses are maintened on the node receiver
	%4.2.1) Add all nodes to the genotype from the phenotypes temporary file
	NodeCollectFun = fun({ElementId, ElementPhenotype}) ->
						element_phenotype_to_genotype(Genotype, ElementPhenotype),
						continue
					end,
	dets:traverse(PhenotypesTemp, NodeCollectFun),
	%4.2.2.) Add all synaspse to the genotype from the phenotypes temporary file
	SynapseCollectFun = fun({ElementId, ElementPhenotype}) ->
							synapse_phenotype_to_genotype(Genotype, ElementPhenotype),
							continue
						end,
	dets:traverse(PhenotypesTemp, SynapseCollectFun),
	%4.3) Delete all unacessary files
	%4.3.1) Delete the phenotype ids dets store
	file:delete(AgentDirectoryPath ++ atom_to_list(AgentId)),
	%4.3.2) Delete the temporary phenotypes store
	file:delete(AgentDirectoryPath ++ "phenotypes_temp"),
	{Agent#agent{elemets_dets = null},Genotype}.

element_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, cortex_phenotype) ->
	#cortex_phenotype{id = Id, fit_directives = FitDirectives, real_directives = RealDirectives} = ElementPhenotype,
	CortexGenotypeLabel = #{id => Id, fit_directives => FitDirectives, real_directives => RealDirectives},
	genotype:add_cortex(Genotype, CortexGenotypeLabel);
element_genotype_to_phenotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, sensor_phenotype) ->
	#sensor_phenotype{id = Id, signal_input_length = Len, fit_directives = FitDirectives, real_directives = RealDirectives} = ElementPhenotype,
	SensorGenotypeLabel = #{id => Id, signal_input_length => Len, fit_directives => FitDirectives, real_directives => RealDirectives},
	genotype:add_sensor(Genotype, SensorGenotypeLabel);
element_genotype_to_phenotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, neuron_classic_phenotype) ->
	#neuron_classic_phenotype{id = Id, layer = Layer, bias = Bias, activation_function = ActivationFunction} = ElementPhenotype,
	NeuronClassicGenotypeLabel = #{id => Id, layer => Layer, bias => Bias, activation_function => ActivationFunction},
	genotype:add_neuron(Genotype, NeuronClassicGenotypeLabel);
element_genotype_to_phenotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, neuron_som_phenotype) ->
	#neuron_som_phenotype{id = Id, coordinates = Coords, cluster = Cluster, weight = Weight, activation_function = ActivationFunction} = ElementPhenotype,
	NeuronSomGenotypeLabel = #{id => Id, cluster => Cluster, coordinates => Coords, weight => Weight, activation_function => ActivationFunction},
	genotype:add_neuron(Genotype, NeuronSomGenotypeLabel);
element_genotype_to_phenotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, actuator_phenotype) ->
	#actuator_phenotype{id = Id, number_of_clients = ClientsNum, fit_directives = FitDirectives, real_directives = RealDirectives} = ElementPhenotype,
	ActuatorGenotypeLabel = #{id => Id, number_of_clients => ClientsNum, fit_directives => FitDirectives, real_directives => RealDirectives},
	genotype:add_actuator(Genotype, ActuatorGenotypeLabel).

% The node receiver maintain information about synapses
% In base at the node type there are synapse without data information(weight, modulation ecc..)
%% Synapses are mapped backwards, that is, given a node, the incoming edges on it are taken.
synapse_phenotype_to_genotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, cortex_phenotype) ->
	#cortex_phenotype{id = Id, input_elements_data = Synapses} = ElementPhenotype,
	% Are taken synapses from actuators to this cortex
	% Cortex synapses don't have any data information, apart the connection direction
	SynapseGenotypeLabelFun = fun({IdFrom, NodeTypeFrom}) -> 
								#{id_from => IdFrom, id_to => Id, tag => {NodeTypeFrom, cortex}, weight => [], plasticity_modulation => none, type => forward} 
							end,
	[genotype:add_synapses(Genotype, SynapseGenotypeLabelFun(Synapse)) || Synapse <- Synapses];
synapse_genotype_to_phenotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, sensor_phenotype) ->
	#sensor_phenotype{id = Id, input_elements_data = Synapses} = ElementPhenotype,
	% Are taken synapses from cortex to this sensor
	% Similar to the cortex, sensors synapses don't have any data information, apart the connection direction
	SynapseGenotypeLabelFun = fun({IdFrom, NodeTypeFrom}) -> 
								#{id_from => IdFrom, id_to => Id, tag => {NodeTypeFrom, sensor}, weight => [], plasticity_modulation => none, type => forward} 
							end,
	[genotype:add_synapses(Genotype, SynapseGenotypeLabelFun(Synapse)) || Synapse <- Synapses];
synapse_genotype_to_phenotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, neuron_classic_phenotype) ->
	#neuron_classic_phenotype{id = Id, input_signals_data = InputSynapses, recurrent_input_signals_data = RecurrentInputSynapses} = ElementPhenotype,
	% Are taken synapses from sensors and other neurons to this neuron
	% Neurons classic synapses have data informations
	SynapseGenotypeLabelFun = fun({IdFrom, NodeTypeFrom, Weight, PlasticityData, Type}) -> 
								#{id_from => IdFrom, id_to => Id, tag => {NodeTypeFrom, neuron}, weight => Weight, plasticity_modulation => Plasticity, type => Type} 
							end,
	% Add forward synapses
	[genotype:add_synapses(Genotype, SynapseGenotypeLabelFun({IdFrom, NodeTypeFrom, Weight, PlasticityData, forward})) || {IdFrom, NodeTypeFrom, Weight, PlasticityData} <- InputSynapses],
	%Add recurrent synapses
	[genotype:add_synapses(Genotype, SynapseGenotypeLabelFun({IdFrom, NodeTypeFrom, Weight, PlasticityData, recurrent})) || {IdFrom, NodeTypeFrom, Weight, PlasticityData} <- RecurrentInputSynapses],;
synapse_genotype_to_phenotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, neuron_som_phenotype) ->
	#neuron_som_phenotype{id = Id, neighbors_data = NeighborsSynapses, input_elements_data = Synapses} = ElementPhenotype,
	% Are taken synapses from sensors and other neurons to this neuron
	% Neurons som synapses dont have data informations apart connection direction
	% 1) Map neighbors synapses
	SynapseNeighborLabelFun = fun({IdFrom, Type}) -> 
								#{id_from => IdFrom, id_to => Id, tag => {neuron, neuron}, weight => [], plasticity_modulation => none, type => Type} 
							end,
	[genotype:add_synapses(Genotype, SynapseNeighborLabelFun(Synapse)) || Synapse <- NeighborsSynapses],
	% 2) Map input synapses
	SynapseGenotypeLabelFun = fun({IdFrom, NodeTypeFrom}) -> 
								#{id_from => IdFrom, id_to => Id, tag => {NodeTypeFrom, neuron}, weight => [], plasticity_modulation => none, type => Type} 
							end,
	[genotype:add_synapses(Genotype, SynapseGenotypeLabelFun(Synapse)) || Synapse <- Synapses];
synapse_genotype_to_phenotype(Genotype, ElementPhenotype) when is_record(ElementPhenotype, actuator_phenotype) ->
	#actuator_phenotype{id = Id, input_elements_data = Synapses} = ElementPhenotype,
	% Are taken synapses from neurons to this actuator
	% Actuator synapses don't have any data information, apart the connection direction
	SynapseGenotypeLabelFun = fun({IdFrom, NodeTypeFrom}) -> 
								#{id_from => IdFrom, id_to => Id, tag => {NodeTypeFrom, actuator}, weight => [], plasticity_modulation => none, type => forward} 
							end,
	[genotype:add_synapses(Genotype, SynapseGenotypeLabelFun(Synapse)) || Synapse <- Synapses].
%%%Stop mapping section

link_to_scape(Agent, ScapeId)->
	#agent{id = Id, phenotype = Phenotype} = Agent,
	[gen_server:call(SensorId, {set_scape, Scape}) || SensorId <- phenotype:get_sensors_ids(Phenotype)],
	[gen_server:call(ActuatorId, {set_scape, Scape}) || ActuatoreId <- phenotype:get_actuators_ids(Phenotype)],
	ok.


%%Given an input and list of function, create an evaluation pipe on the input
apply_directives_pipe(Signal,[])->Signal;
apply_directives_pipe(Signal,[{Mod,Fun,ExtraArgs}|T])when is_atom(Mod),is_atom(Fun),is_list(ExtraArgs)->
	NewSignal=erlang:apply(Mod,Fun,[Signal|ExtraArgs]),
	apply_directives_pipe(NewSignal,T);
apply_directives_pipe(Signal,[{Fun,ExtraArgs}|T])when is_function(Fun),is_list(ExtraArgs)->
	NewSignal=erlang:apply(Fun,[Signal|ExtraArgs]),
	apply_directives_pipe(NewSignal,T).

%Given a list of keys or tuple key and a tuple list, order the TupleList in the order of the keys of SortList
order_by_keylist(SortList, TupleListToOrder)->
	order_by_keylist(SortList, TupleListToOrder, []).
order_by_keylist([], _, Acc) -> Acc;
order_by_keylist([{H, _} | T], TupleListToOrder, Acc)->
	{H, Term, Value} = lists:keyfind(H, 1, TupleListToOrder),
	order_by_keylist(T, TupleListToOrder, Acc ++ [{H, Term, Value}]);
order_by_keylist([H | T], TupleListToOrder, Acc)->
	{H, Term, Value} = lists:keyfind(H, 1, TupleListToOrder),
	order_by_keylist(T, TupleListToOrder, Acc ++ [{H, Term, Value}]).

%%Perform a cycle inside the NN given an instruction( fit, fit_predict), interacting with the scape associated
apply_to_scape(fit, CortexId) ->
	gen_server:cast(CortexId, fit_cycle),
	receive
		{fit, another,_} -> 
			apply_to_scape(fit, CortexId);
		{fit, finish, Msg}->
			Msg
	end;
apply_to_scape(fit_predict, CortexId) ->
	gen_server:cast(CortexId, fit_predict_cycle),
	receive
		{fit_predict, another, Msg} ->
			io:fwrite("~p~n", [Msg]),
			apply_to_scape(fit_predict, CortexId);
		{fit_predict, finish, Msg} -> 
			io:fwrite("~p~n", [Msg])
	end.

%%Given a value Val, it perturb the value and return the perturbed val NewVal.
perturbate(Val)->
	NewVal = ?RAND * ?SAT_LIMIT + Val,
	math_utils:saturate(NewVal, -?SAT_LIMIT, ?SAT_LIMIT).