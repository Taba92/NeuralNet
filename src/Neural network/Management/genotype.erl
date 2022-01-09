-module(genotype).
-export([create_NN/5]).
-export([get_select_on_elements_filtered/3, get_elements_filtered/2, get_element_by_id/2]).
-export([get_sensors/1, get_sensors_ids/1, get_actuators/1, get_actuators_ids/1, get_neurons/1, get_neuron_ids/1, get_cortex/1, get_cortex_id/1, get_synapses/3]).
-export([update_element/3]).
-export([add_sensor/2, add_actuator/2, add_cortex/2, add_neuron/2, add_synapses/4]).
-export([delete_element/2, delete_synapse/3]).
-include("utils.hrl").
-include("genotype.hrl").

%Genotype.network = {digraph, ets_vertices, ets_edges, ets_neightbours, is_cyclic}

get_net_type({ffnn,_,_}) -> classic;
get_net_type({{rnn,_},_,_}) -> classic;
get_net_type({som,_})->som.

create_NN({som,Af},SensorSpec,ActuatorSpec,CortexSpec,Params) ->
	create_SOM({som,Af},SensorSpec,ActuatorSpec,CortexSpec,Params);
create_NN(Constraint,SensorSpec,ActuatorSpec,CortexSpec,Params) ->
	create_CLASSIC(Constraint,SensorSpec,ActuatorSpec,CortexSpec,Params).

create_SOM(Constraint,{SignalInputLength,SFitDirectives,SRealDirectives},{AFitDirectives,ARealDirectives},{CFitDirectives,CRealDirectives},{NumX, NumY})->
	%1) Create genotype
	Genotype = #genotype{network_type = som, network = digraph:new()},
	%2) Add a sensor
	SensorId = add_sensor(Genotype, #{signal_input_length => SignalInputLength, fit_directives =>SFitDirectives, real_directives => SRealDirectives}),
	%3) Add an actuator
	ActuatorId = add_actuator(Genotype, #{number_of_clients => NumX * NumY, fit_directives => AFitDirectives, real_directives => ARealDirectives}),
	%4) Add a cortex
	CortexId = add_cortex(Genotype, #{fit_directives => CFitDirectives, real_directives => CRealDirectives}),
	%5) Add the net of neurons, connecting sensor, neurons and actuator
	create_neuro_net(Constraint, Genotype, {SignalInputLength, {NumX, NumY}}, 0),
	%6) Connect sensor and actuator to the cortex
	SinapsyLabel = #{signal_len => 0, modulation_type => none, connection_direction => forward},
	add_synapses(Genotype, CortexId, SensorId, SinapsyLabel),
	add_synapses(Genotype, ActuatorId, CortexId, SinapsyLabel),
	Genotype.

create_CLASSIC({rnn, ActivationFunction, Plasticity}, SensorSpec, ActuatorSpec, CortexSpec, HiddenLayerDensity) ->
	create_CLASSIC({{rnn, 0}, ActivationFunction, Plasticity}, SensorSpec, ActuatorSpec, CortexSpec, HiddenLayerDensity);
create_CLASSIC(Constraint, {SignalInputLength, SFitDirectives, SRealDirectives }, {NumberInputSignals, AFitDirectives, ARealDirectives}, {CFitDirectives, CRealDirectives}, HiddenLayerDensity) ->
	LayerDensity = HiddenLayerDensity ++ [NumberInputSignals],
	%1) Create genotype
	Genotype = #genotype{network_type = get_net_type(Constraint), network = digraph:new()},
	%2) Add a sensor
	SensorId = add_sensor(Genotype, #{signal_input_length => SignalInputLength, fit_directives => SFitDirectives, real_directives => SRealDirectives}),
	%3) Add an actuator
	ActuatorId = add_actuator(Genotype, #{number_of_clients => NumberInputSignals, fit_directives => AFitDirectives, real_directives => ARealDirectives}),
	%4) Add a cortex
	CortexId = add_cortex(Genotype, #{fit_directives => CFitDirectives, real_directives => CRealDirectives}),
	%5) Add the net of neurons, connecting sensor neurons and actuator together
	create_neuro_net(Constraint, Genotype, LayerDensity, 1),
	%6) Connect sensor and actuator to the cortex
	SinapsyLabel = #{signal_len => 0, modulation_type => none, connection_direction => forward},
	add_synapses(Genotype, CortexId, SensorId, SinapsyLabel),
	add_synapses(Genotype, ActuatorId, CortexId, SinapsyLabel),
	Genotype.

%SOM
%In SOM, edges are not weighted and without neuromodulation!
%First layer creation
create_neuro_net({som , ActivationFunction}, Genotype, {SignalInputLength, {NumX, NumY}}, 0) ->
	% 1) Create and add the first layer column of neurons
	IdsFirstLayer = [add_neuron(Genotype, #{signal_len => SignalInputLength, coordinates => {0, Y}, activation_function => ActivationFunction}) || Y <- lists:seq(0, NumY - 1)],
	% 2) Connect the first layer on the y axis, like a chain.
	connect_on_y(Genotype, IdsFirstLayer),
	% 3) Connect sensor and actuator to the layer.
	[SensorId] = get_sensors_ids(Genotype),
	[ActuatorId] = get_actuators_ids(Genotype),
	SinapsyLabel = #{signal_len => 0, modulation_type => none, connection_direction => forward},
	[add_synapses(Genotype, SensorId, NeuronId, SinapsyLabel) || NeuronId <- IdsFirstLayer],
	[add_synapses(Genotype, NeuronId, ActuatorId, SinapsyLabel) || NeuronId <- IdsFirstLayer],
	create_neuro_net({som, ActivationFunction}, Genotype, {SignalInputLength, {NumX, NumY}}, 1);
%Internal and last layers
create_neuro_net({som, ActivationFunction}, Genotype, {SignalInputLength, {NumX, NumY}}, ColumnNumber)when ColumnNumber < NumY  ->
	% 1) Create and add the current layer column of neurons
	IdsCurrentLayer = [add_neuron(Genotype, #{signal_len => SignalInputLength, coordinates => {ColumnNumber, Y}, activation_function => ActivationFunction}) || Y <- lists:seq(0, NumY - 1)],
	% 2) Connect the current layer on the y axis
	connect_on_y(Genotype, IdsCurrentLayer),
	%3) Connect the current layer to the last layer, coupled on the x axe.
	connect_on_x(Genotype, IdsCurrentLayer),
	% 4) Connect sensor and actuator to the layer.
	[SensorId] = get_sensors_ids(Genotype),
	[ActuatorId] = get_actuators_ids(Genotype),
	SinapsyLabel = #{signal_len => 0, modulation_type => none, connection_direction => forward},
	[add_synapses(Genotype, SensorId, NeuronId, SinapsyLabel) || NeuronId <- IdsCurrentLayer],
	[add_synapses(Genotype, NeuronId, ActuatorId, SinapsyLabel) || NeuronId <- IdsCurrentLayer],
	create_neuro_net({som, ActivationFunction}, Genotype, {SignalInputLength, {NumX , NumY}}, ColumnNumber + 1);
% Stop
create_neuro_net({som, _}, _, {_, {_, NumY}}, ColumnNumber) when ColumnNumber == NumY ->
	ok;
%%

%%FFNN
%1° layer 
create_neuro_net({ffnn, ActivationFunction, Plasticity}, Genotype, [H | OtherLayerDensity], 1) ->
	% 1) Create and add the first layer of neurons
	IdsFirstLayer = [add_neuron(Genotype, #{layer => 1, activation_function => ActivationFunction}) || _ <- lists:seq(1, H)],
	% 2) Connect sensor and the first neuron layer
	[#sensor_genotype{id = SensorId, signal_input_length = SignalInputLength}] = get_sensors(Genotype),
	SinapsyLabel = #{signal_len => SignalInputLength, modulation_type => Plasticity, connection_direction => forward},
	[add_synapses(Genotype, SensorId, NeuronId, SinapsyLabel) || NeuronId <- IdsFirstLayer],
	create_neuro_net({ffnn, ActivationFunction,Plasticity}, Genotype, OtherLayerDensity, 2);
%Internal and last layers
create_neuro_net({ffnn, ActivationFunction, Plasticity}, Genotype, [H | OtherLayerDensity], LayerNum) ->
	% 1) Create and add the layer of neurons
	IdsCurrentLayer = [add_neuron(Genotype, #{layer => LayerNum, activation_function => ActivationFunction}) || _ <- lists:seq(1, H)],
	% 2) Connect the last layer to the current layer of neuron
	% 2.1) Get the ids of the last neuron layer
	SelectFun = fun(#neuron_classic_genotype{id = Id}) -> Id end,
	Predicate = fun(El) -> 
					case is_record(El, neuron_classic_genotype) of
						true -> El#neuron_classic_genotype.layer == LayerNum - 1;
						false -> false
					end 
				end,
	IdsLastLayer = get_select_on_elements_filtered(Genotype, Predicate, SelectFun),
	% 2.2) Add synapses
	SynapsyLabel = #{signal_len => 1, modulation_type => Plasticity, connection_direction => forward},
	[add_synapses(Genotype, LastNeuronId, CurrentNeuronId, SynapsyLabel) || LastNeuronId <- IdsLastLayer, CurrentNeuronId <- IdsCurrentLayer],
	create_neuro_net({ffnn, ActivationFunction, Plasticity}, Genotype, OtherLayerDensity, LayerNum + 1);
% Stop
create_neuro_net({ffnn, _, _}, Genotype, [], LayerLimit) ->
	% 1) Get the actuator id
	[ActuatorId] = get_actuators_ids(Genotype),
	% 2) Get the ids of the last layer
	SelectFun = fun(#neuron_classic_genotype{id = Id}) -> Id end,
	Predicate = fun(El) -> 
					case is_record(El, neuron_classic_genotype) of
						true -> El#neuron_classic_genotype.layer == LayerLimit - 1;
						false -> false
					end 
				end,
	IdsLastLayer = get_select_on_elements_filtered(Genotype, Predicate, SelectFun),
	% 3) Connect the last layer to the actuator
	SinapsyLabel = #{signal_len => 0, modulation_type => none, connection_direction => forward},
	[add_synapses(Genotype, NeuronId, ActuatorId, SinapsyLabel) || NeuronId <- IdsLastLayer],
	ok;
%%

%%RNN
%First layer 
create_neuro_net({{rnn, RecurrentDepthLevel}, ActivationFunction, Plasticity}, Genotype, [H | OtherLayerDensity], 1) ->
	% 1) Create and add the first layer of neurons
	IdsFirstLayer = [add_neuron(Genotype, #{layer => 1, activation_function => ActivationFunction}) || _ <- lists:seq(1, H)],
	% 2) Connect sensor and the first neuron layer
	[#sensor_genotype{id = SensorId, signal_input_length = SignalInputLength}] = get_sensors(Genotype),
	SinapsyLabel = #{signal_len => SignalInputLength, modulation_type => Plasticity, connection_direction => forward},
	[add_synapses(Genotype, SensorId, NeuronId, SinapsyLabel) || NeuronId <- IdsFirstLayer],
	% 3) Connect the first layer with itself
	% 3.1) Foreach id of the first layer, add a synapse between it and itself
	SynapsyLabelRnn = #{signal_len => 1, modulation_type => Plasticity, connection_direction => recurrent},
	[add_synapses(Genotype, CurrentNeuronId, CurrentNeuronId, SynapsyLabelRnn) || CurrentNeuronId <- IdsFirstLayer],
	create_neuro_net({{rnn, RecurrentDepthLevel}, ActivationFunction, Plasticity}, Genotype, OtherLayerDensity, 2);
%Internal and last layer
create_neuro_net({{rnn, RecurrentDepthLevel}, ActivationFunction, Plasticity}, Genotype, [H | OtherLayerDensity], LayerNum) ->
	% 1) Create and add the layer of neurons
	IdsCurrentLayer = [add_neuron(Genotype, #{layer => LayerNum, activation_function => ActivationFunction}) || _ <- lists:seq(1, H)],
	% 2) Connect the last layer to the current layer of neurons
	% 2.1) Get the ids of the last neuron layer
	SelectFun = fun(#neuron_classic_genotype{id = Id}) -> Id end,
	PredicateFfnn = fun(El) -> 
					case is_record(El, neuron_classic_genotype) of
						true -> El#neuron_classic_genotype.layer == LayerNum - 1;
						false -> false
					end 
				end,
	IdsLastLayer = get_select_on_elements_filtered(Genotype, PredicateFfnn, SelectFun),
	% 2.2) Add synapses
	SynapsyLabelFfnn = #{signal_len => 1, modulation_type => Plasticity, connection_direction => forward},
	[add_synapses(Genotype, LastNeuronId, CurrentNeuronId, SynapsyLabelFfnn) || LastNeuronId <- IdsLastLayer, CurrentNeuronId <- IdsCurrentLayer],
	% 3) Connect the current layer to the RecurrentDepthLevel layers of neurons
	% 3.1) Get the ids of the recurrents layers of neurons
	PredicateRnn = fun(El) ->
						case is_record(El, neuron_classic_genotype) of
							true -> 
								Layer = El#neuron_classic_genotype.layer,
								Layer < LayerNum andalso Layer >= LayerNum - RecurrentDepthLevel;
							false -> false
						end 
					end,
	IdsRecurrentLayer = get_select_on_elements_filtered(Genotype, PredicateRnn, SelectFun),
	% 3.2) Foreach id of the current layer, add a synapse between it and every recurrent id.
	SynapsyLabelRnn = #{signal_len => 1, modulation_type => Plasticity, connection_direction => recurrent},
	[add_synapses(Genotype, CurrentNeuronId, RecurrentNeuronId, SynapsyLabelRnn) || CurrentNeuronId <- IdsCurrentLayer, RecurrentNeuronId <- IdsRecurrentLayer],
	% 3.3) Connect the current layer with itself
	% 3.3) Foreach id of the current layer, add a synapse between it and itself
	SynapsyLabelRnn = #{signal_len => 1, modulation_type => Plasticity, connection_direction => recurrent},
	[add_synapses(Genotype, CurrentNeuronId, CurrentNeuronId, SynapsyLabelRnn) || CurrentNeuronId <- IdsCurrentLayer],
	create_neuro_net({{rnn, RecurrentDepthLevel}, ActivationFunction, Plasticity}, Genotype, OtherLayerDensity, LayerNum + 1);
%Stop
create_neuro_net({{rnn, _}, _, _}, Genotype, [], LayerLimit) ->
	create_neuro_net({ffnn, none, none}, Genotype, [], LayerLimit),
	ok.
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SOM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Are added edges for each couple of contiguos neuron on the y axe.
connect_on_y(Genotype, [NeuronId1 | [NeuronId2 | T]]) ->
	SinapsyLabel = #{signal_len => 0, modulation_type => none, connection_direction => recurrent},
	%% Add synapse from NeuronId1 to NeuronId2 
	add_synapses(Genotype, NeuronId1, NeuronId2, SinapsyLabel),
	%% Add synapse from NeuronId2 to NeuronId1 
	add_synapses(Genotype, NeuronId2, NeuronId1, SinapsyLabel),
	connect_on_y(Genotype, [NeuronId2 | T]);
connect_on_y(_, [_ | []]) -> ok.

% Are added edges for each couple of contiguos neuron on the x axe.
connect_on_x(Genotype, [NeuronId | OtherNeuronsIds]) ->
	%Get coordinates of the Neuron
	#neuron_som_genotype{coordinates = {X, Y}} = get_element_by_id(Genotype, NeuronId),
	%Get the id of the neuron on coordinates {X - 1, Y}
	Predicate = fun(Node) ->
					case is_record(Node, neuron_som_genotype) of
						true -> #neuron_som_genotype{coordinates = {X2, Y2}} = Node,
								X2 == X - 1 andalso Y2 == Y;
						false -> false
					end
			end,
	Select = fun(#neuron_som_genotype{id = Neuron2Id}) -> Neuron2Id end,
	[Neuron2Id] = get_select_on_elements_filtered(Genotype, Predicate, Select),
	%Connect the neurons
	add_synapses(Genotype, NeuronId, Neuron2Id, #{signal_len => 0, modulation_type => none, connection_direction => recurrent}),
	add_synapses(Genotype, Neuron2Id, NeuronId, #{signal_len => 0, modulation_type => none, connection_direction => forward}),
	connect_on_x(Genotype, OtherNeuronsIds);
connect_on_x(_, []) -> ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SOM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_elements_filtered(Genotype, Predicate) ->
	{digraph, Vertices, _, _, _} = Genotype#genotype.network,
	%Object in ets tables are stored as {VertexId, VertexValue} pairs 
	Filter = fun({_, El}, Acc) ->
			case Predicate(El) of
				true -> [El | Acc];
				false -> Acc
			end
		end,
	ets:foldl(Filter, [], Vertices).

get_select_on_elements_filtered(Genotype, Predicate, SelectFunction) ->
	{digraph, Vertices, _, _, _} = Genotype#genotype.network,
	Filter = fun({_, El}, Acc) ->
			case Predicate(El) of
				true -> [SelectFunction(El) | Acc];
				false -> Acc
			end
		end,
	ets:foldl(Filter, [], Vertices).

get_element_by_id(Genotype, ElementId) ->
	{ElementId, Data} = digraph:vertex(Genotype#genotype.network, ElementId),
	Data.

get_sensors(Genotype) ->
	Predicate = fun(El) -> is_record(El, sensor_genotype) end,
	get_elements_filtered(Genotype, Predicate).

get_sensors_ids(Genotype) ->
	Predicate = fun(El) -> is_record(El, sensor_genotype) end,
	Select = fun(#sensor_genotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Genotype, Predicate, Select).

get_actuators(Genotype) ->
	Predicate = fun(El) -> is_record(El, actuator_genotype) end,
	get_elements_filtered(Genotype, Predicate).

get_actuators_ids(Genotype) ->
	Predicate = fun(El) -> is_record(El, actuator_genotype) end,
	Select = fun(#actuator_genotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Genotype, Predicate, Select).

get_neuron_ids(Genotype) when Genotype#genotype.network_type == som ->
	Predicate = fun(El) -> is_record(El, neuron_som_genotype) end,
	Select = fun(#neuron_som_genotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Genotype, Predicate, Select);
get_neuron_ids(Genotype) when Genotype#genotype.network_type == classic->
	Predicate = fun(El) -> is_record(El, neuron_classic_genotype) end,
	Select = fun(#neuron_classic_genotype{id = Id}) -> Id end,
	get_select_on_elements_filtered(Genotype, Predicate, Select).

get_neurons(Genotype) when Genotype#genotype.network_type == som->
	Predicate = fun(El) -> is_record(El, neuron_som_genotype) end,
	get_elements_filtered(Genotype, Predicate);
get_neurons(Genotype) when Genotype#genotype.network_type == classic->
	Predicate = fun(El) -> is_record(El, neuron_classic_genotype) end,
	get_elements_filtered(Genotype, Predicate).

get_cortex(Genotype) ->
	Predicate = fun(El) -> is_record(El, cortex_genotype) end,
	[Cortex] = get_elements_filtered(Genotype, Predicate),
	Cortex.

get_cortex_id(Genotype) ->
	Cortex = get_cortex(Genotype),
	#cortex_genotype{id = Id} = Cortex,
	Id.

get_synapses(Genotype, IdFrom, IdTo) ->
	EdgeId = {IdFrom, IdTo},
	{EdgeId, IdFrom, IdTo, EdgeInfo} = digraph:edge(Genotype, EdgeId),
	EdgeInfo.
	
add_synapses(Genotype, IdFrom, IdTo, SinapsesLabel) ->
	#{signal_len := SignalLength, modulation_type := NeuroModulationType, connection_direction := ConnectionDirection} = SinapsesLabel,
	EdgeId = {IdFrom, IdTo},
	Weight = utils:get_random_list(SignalLength),
	Modulation = plasticity:get_plasticity(Weight, NeuroModulationType),
	SinapsesGenotype = #synapses{from = IdFrom, to = IdTo, weight = Weight, plasticity_modulation = Modulation, type = ConnectionDirection},
	digraph:add_edge(Genotype#genotype.network, EdgeId, IdFrom, IdTo, SinapsesGenotype),
	EdgeId.

%Create elements using a new model
add_neuron(Genotype, NeuronSomLabel) when Genotype#genotype.network_type == som ->
	NeuronId = ?GETID,
	#{signal_len := SignalLength, coordinates := Coordinates, activation_function := ActivationFunction} = NeuronSomLabel,
	Weight = utils:get_random_list(SignalLength),
	NeuronSomGenotype = #neuron_som_genotype{id = NeuronId, weight = Weight, coordinates = Coordinates, activation_function = ActivationFunction},
	digraph:add_vertex(Genotype#genotype.network, NeuronId, NeuronSomGenotype),
	NeuronId;
add_neuron(Genotype, NeuronClassicLabel) when Genotype#genotype.network_type == classic ->
	NeuronId = ?GETID,
	#{layer := Layer, activation_function := ActivationFunction} = NeuronClassicLabel,
	Bias = ?RAND,
	NeuronClassicGenotype = #neuron_classic_genotype{id = NeuronId, bias = Bias, layer = Layer, activation_function = ActivationFunction},
	digraph:add_vertex(Genotype#genotype.network, NeuronId, NeuronClassicGenotype),
	NeuronId.

add_cortex(Genotype, CortexLabel) ->
	CortexId = ?GETID,
	#{fit_directives := FitDirectives, real_directives := RealDirectives} = CortexLabel,
	CortexGenotype = #cortex_genotype{id = CortexId, fit_directives = FitDirectives, real_directives = RealDirectives},
	digraph:add_vertex(Genotype#genotype.network, CortexId, CortexGenotype),
	CortexId.

add_sensor(Genotype, SensorLabel) ->
	SensorId = ?GETID,
	#{signal_input_length := SignalInputLen, fit_directives := FitDirectives, real_directives := RealDirectives} = SensorLabel,
	SensorGenotype = #sensor_genotype{id = SensorId, signal_input_length = SignalInputLen, fit_directives = FitDirectives, real_directives = RealDirectives},
	digraph:add_vertex(Genotype#genotype.network, SensorId, SensorGenotype),
	SensorId.

add_actuator(Genotype, ActuatorLabel) ->
	ActuatorId = ?GETID,
	#{number_of_clients := NumberInputSignals, fit_directives := FitDirectives, real_directives := RealDirectives} = ActuatorLabel,
	ActuatorGenotype = #actuator_genotype{id = ActuatorId, number_of_clients = NumberInputSignals, fit_directives = FitDirectives, real_directives = RealDirectives},
	digraph:add_vertex(Genotype#genotype.network, ActuatorId, ActuatorGenotype),
	ActuatorId.

update_element(Genotype, ElementId, ElementLabel) ->
	throw(to_be_implemented). 

delete_element(Genotype, ElementId) ->
	digraph:del_vertex(Genotype#genotype.network, ElementId),
	ok.

delete_synapse(Genotype, IdFrom, IdTo) ->
	EdgeId = {IdFrom, IdTo},
	digraph:del_edge(Genotype#genotype.network, EdgeId),
	ok.



