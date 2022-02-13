-module(agent).
-export([init/1,handle_call/3,terminate/2]).
-include("utils.hrl").
-include("phenotype.hrl").

init([Id, Genotype, Fitness]) ->
	AgentState = ?NN_SERVICE_MODULE:genotype_to_phenotype(Genotype),
	State = AgentState#agent{id = Id, fitness = Fitness},
	{ok, State}.

terminate(normal, State) ->
	#agent{phenotype = Phenotype} = State,
	?NN_SERVICE_MODULE:stop_phenotype(Phenotype).

handle_call({save_nn, FilePath}, _, State) ->
	#agent{id = Id, phenotype = Phenotype, fitness = Fitness} = State,
	#phenotype{network_type = NetType, elements_dets = DetsId} = Phenotype,
	%1) Create the file path if not exist
	filelib:ensure_dir(FilePath),
	%2) Create the file name of the agent
	FileName = atom_to_list(Id) ++ "store.nn",
	%3) Create the dets store file of the agent
	{ok, DetsDump} = dets:open_file(d, [{file, FilePath ++ "/" ++ FileName}, {type, set}]),
	%4) Dump to file the agent data
	%4.1) Dump the agent fitness
	dets:insert(DetsDump, {fitness, Fitness}),
	%4.2) Dump the type of the network
	dets:insert(DetsDump, {network_type, NetType}),
	%4.2) Dump the phenotype of the agent
	PhenotypeStoringFun = fun({ElementId, _}) ->
							ElementPhenotype = phenotype:get_element_by_id(Phenotype, ElementId),
							dets:insert(DetsDump, {ElementId, ElementPhenotype}),
						  	continue
						  end,
	dets:traverse(DetsId, PhenotypeStoringFun),
	dets:close(DetsDump),
	{reply, ok, State};
handle_call({load_nn, FilePath}, _, State) ->
	#agent{id = Id, phenotype = Phenotype} = State,
	%1) Check if the file path exist
	case filelib:is_dir(FilePath) of
		true -> ok;
		false -> throw("Directory" ++ FilePath ++ "not exist. Cannot load the neural network")
	end,
	%2) Check if the file of the agent store exist
	FileName = atom_to_list(Id) ++ "store.nn",
	case filelib:is_file(FilePath ++ "/" ++ FileName) of
		true -> ok;
		false -> throw("File" ++ FilePath ++ "/" ++ FileName ++ "not exist. Cannot load the neural network")
	end,
	%3) Open the agent file dump
	{ok, DetsDump} = dets:open_file(d, [{file, FilePath ++ "/" ++ FileName}, {type, set}]),
	%4) Stop existing phenotype 
	?NN_SERVICE_MODULE:stop_phenotype(Phenotype),
	%5) Start loading
	[{fitness, Fitness}] = dets:lookup(DetsDump, fitness),
	[{network_type, NetType}] = dets:lookup(DetsDump, network_type),
	LoadPhenotypeFun = fun({fitness, _}) ->
							  continue;
						  ({network_type, _}) ->
							  continue;
						  ({_ElementId, ElementPhenotype}) ->
							  phenotype:add_element_with_phenotype(Phenotype, ElementPhenotype),
							  continue
						end,
	dets:traverse(DetsDump, LoadPhenotypeFun),
	NewPhenotype = Phenotype#phenotype{network_type = NetType},
	dets:close(DetsDump),
	{reply, ok, State#agent{phenotype = NewPhenotype, fitness = Fitness}};
handle_call({set_scape, ScapeId}, _, State) ->
	?NN_SERVICE_MODULE:link_to_scape(State, ScapeId),
	{reply, ok, State#agent{scape_id = ScapeId}};
handle_call(fit_predict, _, State)->
	?NN_SERVICE_MODULE:apply_to_scape(fit_predict, State),
	{reply, ok, State};
handle_call({predict, Signal}, _, State)->
	#agent{phenotype = Phenotype} = State,
	CortexId = phenotype:get_cortex_id(Phenotype),
	gen_server:cast(CortexId, {predict_cycle, Signal}),
	receive {prediction, Prediction} -> ok end,
	{reply, Prediction, State};	
handle_call({fit, Params}, _, State)->
	%io:fwrite("GENO FITTING: ~p~n",[State#agent.genotype]),
	NewState = trainer:fit(State,Params),
	{reply, NewState#agent.fitness, NewState};
handle_call(get, _, State) ->
	{reply,State,State}.