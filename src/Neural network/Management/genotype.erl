-module(genotype).
-export([create_NN/5]).
-export([get_layers/1]).
-export([new/1]).
-export([get_select_on_elements_filtered/3, get_elements_filtered/2, get_elements_ids/1, get_element_by_id/2, get_synapses_ids/1]).
-export([get_sensors/1, get_sensors_ids/1, get_actuators/1, get_actuators_ids/1, get_neurons/1, get_neuron_ids/1, get_cortex/1, get_cortex_id/1, get_synapses/3]).
-export([update_element_genotype/3, update_synapse_genotype/4]).
-export([add_sensor/2, add_actuator/2, add_cortex/2, add_neuron/2, add_synapses/4, add_synapses/2, add_element_with_genotype/2]).
-export([delete_element/2, delete_synapse/3, delete_synapses_from/2]).
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
	%6.1) Connect cortex to sensor
	SinapsyCortexToSensorLabel = #{signal_len => 0, tag => {cortex, sensor}, modulation_type => none, connection_direction => forward},
	add_synapses(Genotype, CortexId, SensorId, SinapsyCortexToSensorLabel),
	%6.2) Connect actuator to cortex
	SinapsyActuatorToCortexLabel = #{signal_len => 0, tag => {actuator, cortex}, modulation_type => none, connection_direction => forward},
	add_synapses(Genotype, ActuatorId, CortexId, SinapsyActuatorToCortexLabel),
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
	%6.1) Connect cortex to sensor
	SinapsyCortexToSensorLabel = #{signal_len => 0, modulation_type => none, tag => {cortex, sensor}, connection_direction => forward},
	add_synapses(Genotype, CortexId, SensorId, SinapsyCortexToSensorLabel),
	%6.2) Connect actuator to cortex
	SinapsyActuatorToCortexLabel = #{signal_len => 0, modulation_type => none, tag => {actuator, cortex}, connection_direction => forward},
	add_synapses(Genotype, ActuatorId, CortexId, SinapsyActuatorToCortexLabel),
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
	%3.1) Connect sensor to the layer
	SinapsySensorToNeuronLabel = #{signal_len => 0, tag => {sensor, neuron}, modulation_type => none, connection_direction => forward},
	[add_synapses(Genotype, SensorId, NeuronId, SinapsySensorToNeuronLabel) || NeuronId <- IdsFirstLayer],
	%3.2) Connect layer to the actuator
	SinapsyNeuronToActuatorLabel = #{signal_len => 0, tag => {neuron, actuator}, modulation_type => none, connection_direction => forward},
	[add_synapses(Genotype, NeuronId, ActuatorId, SinapsyNeuronToActuatorLabel) || NeuronId <- IdsFirstLayer],
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
	% 4.1) Connect sensor to the layer
	SinapsySensorToNeuronLabel = #{signal_len => 0, tag => {sensor, neuron}, modulation_type => none, connection_direction => forward},
	[add_synapses(Genotype, SensorId, NeuronId, SinapsySensorToNeuronLabel) || NeuronId <- IdsCurrentLayer],
	% 4.2) Connect layer to actuator
	SinapsyNeuronToActuatorLabel = #{signal_len => 0, tag => {neuron, actuator}, modulation_type => none, connection_direction => forward},
	[add_synapses(Genotype, NeuronId, ActuatorId, SinapsyNeuronToActuatorLabel) || NeuronId <- IdsCurrentLayer],
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
	SinapsySensorToNeuronLabel = #{signal_len => SignalInputLength, tag => {sensor, neuron}, modulation_type => Plasticity, connection_direction => forward},
	[add_synapses(Genotype, SensorId, NeuronId, SinapsySensorToNeuronLabel) || NeuronId <- IdsFirstLayer],
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
	SynapsyNeuronToNeuronLabel = #{signal_len => 1, tag => {neuron, neuron}, modulation_type => Plasticity, connection_direction => forward},
	[add_synapses(Genotype, LastNeuronId, CurrentNeuronId, SynapsyNeuronToNeuronLabel) || LastNeuronId <- IdsLastLayer, CurrentNeuronId <- IdsCurrentLayer],
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
	SinapsyNeuronToActuatorLabel = #{signal_len => 0, tag => {neuron, actuator}, modulation_type => none, connection_direction => forward},
	[add_synapses(Genotype, NeuronId, ActuatorId, SinapsyNeuronToActuatorLabel) || NeuronId <- IdsLastLayer],
	ok;
%%

%%RNN
%First layer 
create_neuro_net({{rnn, RecurrentDepthLevel}, ActivationFunction, Plasticity}, Genotype, [H | OtherLayerDensity], 1) ->
	% 1) Create and add the first layer of neurons
	IdsFirstLayer = [add_neuron(Genotype, #{layer => 1, activation_function => ActivationFunction}) || _ <- lists:seq(1, H)],
	% 2) Connect sensor and the first neuron layer
	[#sensor_genotype{id = SensorId, signal_input_length = SignalInputLength}] = get_sensors(Genotype),
	SinapsySensorToNeuronLabel = #{signal_len => SignalInputLength, tag => {sensor, neuron}, modulation_type => Plasticity, connection_direction => forward},
	[add_synapses(Genotype, SensorId, NeuronId, SinapsySensorToNeuronLabel) || NeuronId <- IdsFirstLayer],
	% 3) Connect the first layer with itself
	% 3.1) Foreach id of the first layer, add a synapse between it and itself
	SynapsyLabelRnn = #{signal_len => 1, tag => {neuron, neuron}, modulation_type => Plasticity, connection_direction => recurrent},
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
	SynapsyLabelFfnn = #{signal_len => 1, tag => {neuron, neuron}, modulation_type => Plasticity, connection_direction => forward},
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
	SynapsyLabelRnn = #{signal_len => 1, tag => {neuron, neuron}, modulation_type => Plasticity, connection_direction => recurrent},
	[add_synapses(Genotype, CurrentNeuronId, RecurrentNeuronId, SynapsyLabelRnn) || CurrentNeuronId <- IdsCurrentLayer, RecurrentNeuronId <- IdsRecurrentLayer],
	% 3.3) Connect the current layer with itself
	% 3.3) Foreach id of the current layer, add a synapse between it and itself
	SynapsyLabelRnn = #{signal_len => 1, tag => {neuron, neuron}, modulation_type => Plasticity, connection_direction => recurrent},
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
	SinapsyLabel = #{signal_len => 0, tag => {neuron, neuron}, modulation_type => none, connection_direction => recurrent},
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
	add_synapses(Genotype, NeuronId, Neuron2Id, #{signal_len => 0, tag => {neuron, neuron}, modulation_type => none, connection_direction => recurrent}),
	add_synapses(Genotype, Neuron2Id, NeuronId, #{signal_len => 0, tag => {neuron, neuron}, modulation_type => none, connection_direction => forward}),
	connect_on_x(Genotype, OtherNeuronsIds);
connect_on_x(_, []) -> ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SOM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%API high level
get_layers(Genotype) ->
	Predicate = fun(ElementGenotype) -> is_record(ElementGenotype, neuron_classic_genotype) orelse is_record(ElementGenotype, neuron_som_genotype) end,
	SelectFun = fun(NeuronGenotype) ->
					case NeuronGenotype of
						#neuron_classic_genotype{layer = Layer} -> Layer;
						#neuron_som_genotype{coordinates = {X, _}} -> X
					end
				end,
	Layers = lists:usort(get_select_on_elements_filtered(Genotype, Predicate, SelectFun)),
	Layers.

%%API low level
new(NetworkType) ->
	Digraph = digraph:new(),
	#genotype{network_type = NetworkType, network = Digraph}.

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

get_elements_ids(Genotype) ->
	digraph:vertices(Genotype#genotype.network).

get_element_by_id(Genotype, ElementId) ->
	case digraph:vertex(Genotype#genotype.network, ElementId) of
		{ElementId, Data} -> Data;
		false -> false
	end.

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

get_synapses_ids(Genotype) ->
	digraph:edges(Genotype#genotype.network).

get_synapses(Genotype, IdFrom, IdTo) ->
	EdgeId = {IdFrom, IdTo},
	case digraph:edge(Genotype#genotype.network, EdgeId) of
		{EdgeId, IdFrom, IdTo, EdgeInfo} -> EdgeInfo;
		false -> false
	end.

%Given existing genotype node data, add it to the genotype data structure
add_element_with_genotype(Genotype, ElementGenotype) when is_record(ElementGenotype, cortex_genotype) ->
	#cortex_genotype{id = CortexId} = ElementGenotype,
	digraph:add_vertex(Genotype#genotype.network, CortexId, ElementGenotype);
add_element_with_genotype(Genotype, ElementGenotype) when is_record(ElementGenotype, sensor_genotype) ->
	#sensor_genotype{id = SensorId} = ElementGenotype,
	digraph:add_vertex(Genotype#genotype.network, SensorId, ElementGenotype);
add_element_with_genotype(Genotype, ElementGenotype) when is_record(ElementGenotype, actuator_genotype) ->
	#actuator_genotype{id = ActuatorId} = ElementGenotype,
	digraph:add_vertex(Genotype#genotype.network, ActuatorId, ElementGenotype);
add_element_with_genotype(Genotype, ElementGenotype) when is_record(ElementGenotype, neuron_classic_genotype) ->
	#neuron_classic_genotype{id = NeuronId} = ElementGenotype,
	digraph:add_vertex(Genotype#genotype.network, NeuronId, ElementGenotype);
add_element_with_genotype(Genotype, ElementGenotype) when is_record(ElementGenotype, neuron_som_genotype) ->
	#neuron_som_genotype{id = NeuronId} = ElementGenotype,
	digraph:add_vertex(Genotype#genotype.network, NeuronId, ElementGenotype);
add_element_with_genotype(Genotype, ElementGenotype) when is_record(ElementGenotype, synapses) ->
	#synapses{id_from = IdFrom, id_to = IdTo} = ElementGenotype,
	EdgeId = {IdFrom, IdTo},
	digraph:add_edge(Genotype#genotype.network, EdgeId, IdFrom, IdTo, ElementGenotype).


%Create synapse element, generating a new model from the Label	
add_synapses(Genotype, IdFrom, IdTo, #{signal_len := SignalLength, tag := Tag, modulation_type := NeuroModulationType, connection_direction := ConnectionDirection}) ->
	EdgeId = {IdFrom, IdTo},
	Weight = utils:get_random_list(SignalLength),
	Modulation = plasticity:get_plasticity(Weight, NeuroModulationType),
	SinapsesGenotype = #synapses{id_from = IdFrom, id_to = IdTo, tag = Tag, weight = Weight, plasticity_modulation = Modulation, connection_direction = ConnectionDirection},
	digraph:add_edge(Genotype#genotype.network, EdgeId, IdFrom, IdTo, SinapsesGenotype),
	EdgeId.
%Create synapse element, using existing synapse model given in the Label
add_synapses(Genotype, #{id_from := IdFrom, id_to := IdTo, tag := Tag, weight := Weight, plasticity_modulation := Modulation, type := ConnectionDirection}) ->
	EdgeId = {IdFrom, IdTo},
	SinapsesGenotype = #synapses{id_from = IdFrom, id_to = IdTo, tag = Tag, weight = Weight, plasticity_modulation = Modulation, connection_direction = ConnectionDirection},
	digraph:add_edge(Genotype#genotype.network, EdgeId, IdFrom, IdTo, SinapsesGenotype),
	EdgeId.


%Create neuron element, using existing model given in the Label
add_neuron(Genotype, #{id := NeuronId, weight := Weight, coordinates := Coordinates, cluster := Cluster, activation_function := ActivationFunction}) when Genotype#genotype.network_type == som ->
	NeuronSomGenotype = #neuron_som_genotype{id = NeuronId, weight = Weight, cluster = Cluster, coordinates = Coordinates, activation_function = ActivationFunction},
	digraph:add_vertex(Genotype#genotype.network, NeuronId, NeuronSomGenotype),
	NeuronId;
add_neuron(Genotype, #{id := NeuronId, bias := Bias, layer := Layer, activation_function := ActivationFunction}) when Genotype#genotype.network_type == classic ->
	NeuronClassicGenotype = #neuron_classic_genotype{id = NeuronId, bias = Bias, layer = Layer, activation_function = ActivationFunction},
	digraph:add_vertex(Genotype#genotype.network, NeuronId, NeuronClassicGenotype),
	NeuronId;
%Create neuron element, generating a new model from the Label
add_neuron(Genotype, #{signal_len := SignalLength, coordinates := Coordinates, activation_function := ActivationFunction}) when Genotype#genotype.network_type == som ->
	NeuronId = ?GETID,
	Weight = utils:get_random_list(SignalLength),
	NeuronSomGenotype = #neuron_som_genotype{id = NeuronId, weight = Weight, coordinates = Coordinates, activation_function = ActivationFunction},
	digraph:add_vertex(Genotype#genotype.network, NeuronId, NeuronSomGenotype),
	NeuronId;
add_neuron(Genotype, #{layer := Layer, activation_function := ActivationFunction}) when Genotype#genotype.network_type == classic ->
	NeuronId = ?GETID,
	Bias = ?RAND,
	NeuronClassicGenotype = #neuron_classic_genotype{id = NeuronId, bias = Bias, layer = Layer, activation_function = ActivationFunction},
	digraph:add_vertex(Genotype#genotype.network, NeuronId, NeuronClassicGenotype),
	NeuronId.

%Create cortex element, using existing  element model from the Label
add_cortex(Genotype, #{id := CortexId, fit_directives := FitDirectives, real_directives := RealDirectives}) ->
	CortexGenotype = #cortex_genotype{id = CortexId, fit_directives = FitDirectives, real_directives = RealDirectives},
	digraph:add_vertex(Genotype#genotype.network, CortexId, CortexGenotype),
	CortexId;
%Create cortex element, generating a new model from the Label
add_cortex(Genotype, #{fit_directives := FitDirectives, real_directives := RealDirectives}) ->
	CortexId = ?GETID,
	CortexGenotype = #cortex_genotype{id = CortexId, fit_directives = FitDirectives, real_directives = RealDirectives},
	digraph:add_vertex(Genotype#genotype.network, CortexId, CortexGenotype),
	CortexId.

%Create sensor element, using an existing model given in the Label
add_sensor(Genotype, #{id := SensorId, signal_input_length := SignalInputLen, fit_directives := FitDirectives, real_directives := RealDirectives}) ->
	SensorGenotype = #sensor_genotype{id = SensorId, signal_input_length = SignalInputLen, fit_directives = FitDirectives, real_directives = RealDirectives},
	digraph:add_vertex(Genotype#genotype.network, SensorId, SensorGenotype),
	SensorId;
%Create sensor element, generating a new model from the Label
add_sensor(Genotype, #{signal_input_length := SignalInputLen, fit_directives := FitDirectives, real_directives := RealDirectives}) ->
	SensorId = ?GETID,
	SensorGenotype = #sensor_genotype{id = SensorId, signal_input_length = SignalInputLen, fit_directives = FitDirectives, real_directives = RealDirectives},
	digraph:add_vertex(Genotype#genotype.network, SensorId, SensorGenotype),
	SensorId.

%Create actuator element, using an existing model given in the Label
add_actuator(Genotype, #{id := ActuatorId, number_of_clients := NumberInputSignals, fit_directives := FitDirectives, real_directives := RealDirectives}) ->
	ActuatorGenotype = #actuator_genotype{id = ActuatorId, number_of_clients = NumberInputSignals, fit_directives = FitDirectives, real_directives = RealDirectives},
	digraph:add_vertex(Genotype#genotype.network, ActuatorId, ActuatorGenotype),
	ActuatorId;
%Create actuator element, generating a new model from the Label
add_actuator(Genotype, #{number_of_clients := NumberInputSignals, fit_directives := FitDirectives, real_directives := RealDirectives}) ->
	ActuatorId = ?GETID,
	ActuatorGenotype = #actuator_genotype{id = ActuatorId, number_of_clients = NumberInputSignals, fit_directives = FitDirectives, real_directives = RealDirectives},
	digraph:add_vertex(Genotype#genotype.network, ActuatorId, ActuatorGenotype),
	ActuatorId.

update_element_genotype(Genotype, ElementId, NewElementGenotype) ->
	digraph:add_vertex(Genotype#genotype.network, ElementId, NewElementGenotype).

update_synapse_genotype(Genotype, IdFrom, IdTo, NewSynapseGenotype) ->
	digraph:add_edge(Genotype#genotype.network, {IdFrom, IdTo}, IdFrom, IdTo, NewSynapseGenotype).

delete_element(Genotype, ElementId) ->
	digraph:del_vertex(Genotype#genotype.network, ElementId),
	ok.

%Delete synapse outgoing from the node IdFrom
delete_synapses_from(Genotype, IdFrom) ->
	OutgoingSynapses = digraph:out_edges(Genotype#genotype.network, IdFrom),
	digraph:del_edges(Genotype#genotype.network, OutgoingSynapses).

delete_synapse(Genotype, IdFrom, IdTo) ->
	EdgeId = {IdFrom, IdTo},
	digraph:del_edge(Genotype#genotype.network, EdgeId),
	ok.



