-module(nn_service).
-export([genotype_to_phenotype/1, phenotype_to_genotype/2]).
-export([apply_directives_pipe/2, order_by_keylist/2, apply_to_scape/2, perturbate/1]).
-include("utils.hrl").
-include("genotype.hrl").
-include("phenotype.hrl").

get_neuron_phenotype_module(normal) -> neuron;
get_neuron_phenotype_module(som) -> neuron_som.

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
	%4.2) Do the mapping
	%4.2.1) Launch all nodes
	NodesLaunchingFun = fun({ElementId, ElementGenotype}) ->
							element_genotype_to_phenotype(Phenotype, ElementGenotype),
							%Store the element id on the phenotype's dets
							dets:insert(PhenotypeDets, {ElementId, local}),
							continue,
						end,
	dets:traverse(VerticesTemp, NodesLaunchingFun),
	%4.2.3) Connect all nodes, launching all synapses
	EdgesLaunchingFun = fun({SynapseId, SynapseGenotype}) ->
							synapse_genotype_to_phenotype(Phenotype, SynapseGenotype),
							continue
						end,
	dets:traverse(EdgesTemp, EdgesLaunchingFun),
	%5) Delete temporary files
	file:delete(AgentDirectoryPath ++ "vertices_temp"),
	file:delete(AgentDirectoryPath ++ "edges_temp")
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

phenotype_to_genotype(Agent, Phenotype) when is_record(Agent, agent),is_record(Phenotype, phenotype) ->
	CortexGeno=gen_server:call(CortexId,dump,infinity),
	#cortex_phenotype{sensorsIds=SensorsIds,neuronsIds=NeuronsIds,actuatorsIds=ActuatorsIds}=CortexGeno,
	SensorsGeno=[gen_server:call(SensorId,dump,infinity)||SensorId<-SensorsIds],
	NeuronsGeno=[gen_server:call(NeuronId,dump,infinity)||NeuronId<-NeuronsIds],
	ActuatorsGeno=[gen_server:call(ActuatorId,dump,infinity)||ActuatorId<-ActuatorsIds],
	#genotype{sensors=SensorsGeno,neurons=lists:keysort(3,NeuronsGeno),actuators=ActuatorsGeno,cortex=CortexGeno}.

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

%Given a list of keys and a tuple list, order the TupleList in the order of the keys of SortList
order_by_keylist(SortList,TupleListToOrder)->
	order_by_keylist(SortList,TupleListToOrder,[]).
order_by_keylist([],_,Acc)->Acc;
order_by_keylist([H|T],TupleListToOrder,Acc)->
	{H,Term,Value}=lists:keyfind(H,1,TupleListToOrder),
	order_by_keylist(T,TupleListToOrder,Acc++[{H,Term,Value}]).

%%Perform a cycle inside the NN given an instruction( fit, fit_predict)
apply_to_scape(fit,CortexId)->
	gen_server:cast(CortexId,fit_cycle),
	receive
		{fit,another,_}->apply_to_scape(fit,CortexId);
		{fit,finish,Msg}->
			Msg
	end;
apply_to_scape(fit_predict,CortexId)->
	gen_server:cast(CortexId,fit_predict_cycle),
	receive
		{fit_predict,another,Msg}->
			io:fwrite("~p~n",[Msg]),
			apply_to_scape(fit_predict,CortexId);
		{fit_predict,finish,Msg}->io:fwrite("~p~n",[Msg])
	end.

%%Given a value Val, it perturb the value and return the perturbed val NewVal.
perturbate(Val)->
	NewVal = ?RAND * ?SAT_LIMIT + Val,
	math_utils:saturate(NewVal, -?SAT_LIMIT, ?SAT_LIMIT).