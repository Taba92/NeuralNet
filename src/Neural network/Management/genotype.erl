-module(genotype).
-export([create_NN/5]).
%-export([get_cortex_id/1,get_neurons_ids/1,get_sensors_ids/1,get_actuators_ids/1,get_ids/1]).
-export([create_neuron/2,create_weight/2,get_layers/1,get_geno_spec/1]).
-define(LAYER(Neurons),(hd(Neurons))#neuron.layer).
-define(NIDS(Neurons),[NeuronId||#neuron{id=NeuronId}<-Neurons]).%%get neurons ids from a list of neurons
-include("utils.hrl").

%Genotype = {digraph, ets_vertices, ets_edges, ets_neightbours, is_cyclic}
get_geno_spec(Genotype)->
	#genotype{sensors=[S],actuators=[A], cortex = C} = Genotype,
	#sensor{vl=SVl,fit_directives=SFit,real_directives=SReal}=S,
	#actuator{vl=AVl,fit_directives=AFit,real_directives=AReal}=A,
	#cortex{fit_directives=CFit,real_directives=CReal} = C,
	{{SVl,SFit,SReal},{AVl,AFit,AReal},{CFit,CReal},lists:droplast(get_layers(Genotype))}.

get_layers(Genotype)->
	#genotype{neurons=Net}=Genotype,
	Layer=fun(#neuron{layer=L},Layers)->case L>lists:last(Layers) of true->Layers++[L];false->Layers end end,
	erlang:tl(lists:foldl(Layer,[0],Net)).


get_net_type({ffnn,_,_}) -> normal;
get_net_type({{rnn,_},_,_}) -> normal;
get_net_type({som,_})->som.

create_NN({som,Af},SensorSpec,ActuatorSpec,CortexSpec,Params) ->
	create_SOM({som,Af},SensorSpec,ActuatorSpec,CortexSpec,Params);
create_NN(Constraint,SensorSpec,ActuatorSpec,CortexSpec,Params) ->
	create_CLASSIC(Constraint,SensorSpec,ActuatorSpec,CortexSpec,Params).

create_SOM(Constraint,{SensorVl,SFitDirectives,SRealDirectives},{AFitDirectives,ARealDirectives},{CFitDirectives,CRealDirectives},{NumX,NumY})->
	Genotype = digraph:new(),
	Sensor = #sensor{id = ?GETID, vl = SensorVl, fit_directives = SFitDirectives, real_directives = SRealDirectives},
	Actuator = #actuator{id = ?GETID, vl = NumX * NumY, fit_directives = AFitDirectives, real_directives = ARealDirectives},
	digraph:add_vertex(Genotype, Sensor#sensor.id, Sensor),
	digraph:add_vertex(Genotype, Actuator#actuator.id, Actuator),
	create_neuro_net(Constraint, Genotype, {NumX,NumY}, 0),
	%SensorsIds=[SId||#sensor{id=SId}<-[Sensor]],
	%NetIds=[NId||#neuron_som{id=NId}<-Net],
	%ActuatorsIds=[AId||#actuator{id=AId}<-[Actuator]],
	%Cortex=#cortex{id=?GETID,fit_directives=CFitDirectives,real_directives=CRealDirectives,sensorsIds=SensorsIds,neuronsIds=NetIds,actuatorsIds=ActuatorsIds},
	%NewActuator=ConnActuator#actuator{cortexId=Cortex#cortex.id},
	%#genotype{type=get_net_type(Constraint),sensors=[ConnSensor],neurons=Net,actuators=[NewActuator],cortex=Cortex}.
	Genotype.

create_CLASSIC({rnn,Af,Plast},SensorSpec,ActuatorSpec,CortexSpec,HiddenLayerDensity)->
	create_CLASSIC({{rnn,0},Af,Plast},SensorSpec,ActuatorSpec,CortexSpec,HiddenLayerDensity);
create_CLASSIC(Constraint,{SensorVl,SFitDirectives,SRealDirectives},{ActuatorVl,AFitDirectives,ARealDirectives},{CFitDirectives,CRealDirectives},HiddenLayerDensity) ->
	LayerDensity=HiddenLayerDensity++[ActuatorVl],
	Sensor=#sensor{id=?GETID,vl=SensorVl,fit_directives=SFitDirectives,real_directives=SRealDirectives},
	Actuator=#actuator{id=?GETID,vl=ActuatorVl,fit_directives=AFitDirectives,real_directives=ARealDirectives},
	{ConnSensor,Net,ConnActuator}=create_neuro_net(Constraint,{Sensor,[],Actuator},LayerDensity,1),
	SensorsIds=[SId||#sensor{id=SId}<-[Sensor]],
	NetIds=[NId||#neuron{id=NId}<-Net],
	ActuatorsIds=[AId||#actuator{id=AId}<-[Actuator]],
	Cortex=#cortex{id=?GETID,fit_directives=CFitDirectives,real_directives=CRealDirectives,sensorsIds=SensorsIds,neuronsIds=NetIds,actuatorsIds=ActuatorsIds},
	NewActuator=ConnActuator#actuator{cortexId=Cortex#cortex.id},
	#genotype{type=get_net_type(Constraint),sensors=[ConnSensor],neurons=Net,actuators=[NewActuator],cortex=Cortex}.

%som
create_neuro_net({som , ActivationFunction}, {Genotype, InputLength}, {NumX, NumY}, 0) ->
	% Create the first layer of neurons
	FirstLayer = [create_neuron_som(ActivationFunction, 0, Y) || Y <- lists:seq(0, NumY - 1)],
	% Add the first layer in the genotype
	[digraph:add_vertex(Neuron#neuron_som.id, Neuron) || Neuron <- FirstLayer],
	% Connect the first layer on the y axis
	
	ConnNet = connect_on_x(FirstLayer),
	create_neuro_net({som, ActivationFunction},{Genotype, InputLength}, {NumX, NumY}, 1);
create_neuro_net({som,Af},{Sensor,Net,Actuator},{NumX,NumY},X)when X<NumX-1->
	CurrentLayerNet=[create_neuron_som(Af,X,Y)||Y<-lists:seq(0,NumY-1)],
	ConnCurrent=connect_on_x(CurrentLayerNet),
	{UpdateNet,ConnNet}=connect_on_y(Net,ConnCurrent,X),
	create_neuro_net({som,Af},{Sensor,UpdateNet++ConnNet,Actuator},{NumX,NumY},X+1);
create_neuro_net({som,Af},{Sensor,Net,Actuator},{NumX,NumY},X)when X==NumX-1->
	CurrentLayerNet=[create_neuron_som(Af,X,Y)||Y<-lists:seq(0,NumY-1)],
	ConnCurrent=connect_on_x(CurrentLayerNet),
	{UpdateNet,ConnNet}=connect_on_y(Net,ConnCurrent,X),
	ConnSensor=Sensor#sensor{fanouts=[Id||#neuron_som{id=Id}<-UpdateNet++ConnNet]},
	ConnActuator=Actuator#actuator{fanins=[Id||#neuron_som{id=Id}<-UpdateNet++ConnNet]},
	{ConnSensor,UpdateNet++ConnNet,ConnActuator};
%%
%%rnn
create_neuro_net({{rnn,N},Af,Plast},{Sensor,[],Actuator},[H|OtherLayer],1)->%%1° layer RNN
	FirstLayerNet=[create_neuron(Af,1)||_<-lists:seq(1,H)],
	{ConnSensor,ConnFirstLayer}=connect_RNN(N,Sensor,Plast,FirstLayerNet),
	create_neuro_net({{rnn,N},Af,Plast},{ConnSensor,ConnFirstLayer,Actuator},OtherLayer,2);
create_neuro_net({{rnn,N},Af,Plast},{Sensor,Net,Actuator},[H|OtherLayer],LayerNum)->%%layers interni
	CurrentLayerNet=[create_neuron(Af,LayerNum)||_<-lists:seq(1,H)],
	{UpdatedNet,ConnCurrent}=connect_RNN(N,Net,Plast,CurrentLayerNet),%connetto l'ultimo strato con lo strato appena creato
	create_neuro_net({{rnn,N},Af,Plast},{Sensor,UpdatedNet++ConnCurrent,Actuator},OtherLayer,LayerNum+1);
create_neuro_net({{rnn,_},_,Plast},{Sensor,Net,Actuator},[],N)->%%all'ultimo layer
	{UpdatedNet,ConnActuator}=connect_FFNN({N,Net},Plast,Actuator),
	{Sensor,UpdatedNet,ConnActuator};
%%
%%ffnn
create_neuro_net({ffnn,Af,Plast},{Sensor,[],Actuator},[H|OtherLayer],1)->%%1° layer FFNN
	FirstLayerNet=[create_neuron(Af,1)||_<-lists:seq(1,H)],
	{ConnSensor,ConnFirstLayer}=connect_FFNN(Sensor,Plast,FirstLayerNet),
	create_neuro_net({ffnn,Af,Plast},{ConnSensor,ConnFirstLayer,Actuator},OtherLayer,2);
create_neuro_net({ffnn,Af,Plast},{Sensor,Net,Actuator},[H|OtherLayer],LayerNum)->%%layers interni
	CurrentLayerNet=[create_neuron(Af,LayerNum)||_<-lists:seq(1,H)],
	{UpdatedNet,ConnCurrent}=connect_FFNN(Net,Plast,CurrentLayerNet),%connetto l'ultimo strato con lo strato appena creato
	create_neuro_net({ffnn,Af,Plast},{Sensor,UpdatedNet++ConnCurrent,Actuator},OtherLayer,LayerNum+1);
create_neuro_net({ffnn,_,Plast},{Sensor,Net,Actuator},[],N)->%%all'ultimo layer
	{UpdatedNet,ConnActuator}=connect_FFNN({N,Net},Plast,Actuator),
	{Sensor,UpdatedNet,ConnActuator}.
%%


connect_on_x(Net)->[connect_x(Neuron,Net)||Neuron<-Net].
connect_x(Neuron,Net)->
	#neuron_som{coordinates={_,Y}}=Neuron,
	Pred=fun(#neuron_som{coordinates={_,NY}})->NY==Y-1 orelse NY==Y+1 end,
	NeighborsIds=[N#neuron_som.id||N<-lists:filter(Pred,Net)],
	Neuron#neuron_som{neighbors=NeighborsIds}.

connect_on_y(OtherNeurons,LastsNeurons,CurX)->
	Pred=fun(#neuron_som{coordinates={X,_}})->X/=CurX-1 end,
	{OtherLayers,LastLayer}=lists:partition(Pred,OtherNeurons),
	{LastConnLayer,LastsConnNeurons}=connect_y(LastLayer,LastsNeurons,[],[]),
	{OtherLayers++LastConnLayer,LastsConnNeurons}.

connect_y([Last|OtherLayer],[Neuron|OtherLastsNeurons],AccLast,AccCur)->
	#neuron_som{id=LastId,neighbors=LastNeighbors}=Last,
	#neuron_som{id=NeuronId,neighbors=NeuronNeighbors}=Neuron,
	NewLast=Last#neuron_som{neighbors=LastNeighbors++[NeuronId]},
	NewNeuron=Neuron#neuron_som{neighbors=NeuronNeighbors++[LastId]},
	connect_y(OtherLayer,OtherLastsNeurons,AccLast++[NewLast],AccCur++[NewNeuron]);
connect_y([],[],AccLast,AccCur)->{AccLast,AccCur}.

	
connect_FFNN(Sensor,Plast,FirstLayer)when is_record(Sensor,sensor)->
	ConnFirstLayer=[Neuron#neuron{faninsWeights=[create_weight(Plast,Sensor)]}||Neuron<-FirstLayer],%per ogni neurone mi crei il peso del sensore
	ConnSensor=Sensor#sensor{fanouts=?NIDS(FirstLayer)},
	{ConnSensor,ConnFirstLayer};
connect_FFNN(OtherLayers,Plast,LastLayer)when is_record(hd(OtherLayers),neuron)->
	A=fun(#neuron{layer=Layer})->Layer/=?LAYER(LastLayer)-1 end,
	{OldLayers,PenultimateLayer}=lists:partition(A,OtherLayers),%divido il penultimo layer dai restanti layer
	ConnPenultimate=[Neuron#neuron{fanouts=?NIDS(LastLayer)}||Neuron<-PenultimateLayer],%connetto il penultimo strato con l'ultimo
	ConnLast=[Neuron#neuron{faninsWeights=[create_weight(Plast,N)||N<-PenultimateLayer]}||Neuron<-LastLayer],%connetto in entrata l'ultimo strato con il penultimo
	{OldLayers++ConnPenultimate,ConnLast};
connect_FFNN({MaxIndexLayer,Net},_,Actuator)when is_record(Actuator,actuator)->
	A=fun(#neuron{layer=Layer})->Layer/=MaxIndexLayer-1 end,
	{OtherLayers,LastLayer}=lists:partition(A,Net),%divido l'ultimo layer dai restanti layer
	ConnLast=[Neuron#neuron{fanouts=[Actuator#actuator.id]}||Neuron<-LastLayer],%connetto l'ultimo strato con l'attuator
	ConnActuator=Actuator#actuator{fanins=?NIDS(LastLayer)},%connetto in entrata l'attuato all'ultimo strato
	{OtherLayers++ConnLast,ConnActuator}.

connect_RNN(_,Sensor,Plast,FirstLayer)when is_record(Sensor,sensor)->
	{ConnSensor,ConnFirstLayer}=connect_FFNN(Sensor,Plast,FirstLayer),
	SelfFirst=[Neuron#neuron{roinsWeights=[create_weight(Plast,Neuron)],roouts=[Neuron#neuron.id]}||Neuron<-ConnFirstLayer],
	{ConnSensor,SelfFirst};
connect_RNN(D,OtherLayers,Plast,LastLayer)when is_record(hd(OtherLayers),neuron)->
	SelfLastLayer=[N#neuron{roinsWeights=[create_weight(Plast,N)],roouts=[N#neuron.id]}||N<-LastLayer],
	{NewNet,ConnSelfLast}=connect_FFNN(OtherLayers,Plast,SelfLastLayer),
	A=fun(#neuron{layer=Layer})->Layer<?LAYER(LastLayer)-D end,% scremo tutti quelli che non avranno connessioni ricorrenti in entrata dall'ultimo layer creato
	{OldLayers,RoLayers}=lists:partition(A,NewNet),
	RoLastLayer=[Neuron#neuron{roouts=Neuron#neuron.roouts++?NIDS(RoLayers)}||Neuron<-ConnSelfLast],%%connetto in uscita ricorrente lo strato appena creato
	RoConnLayers=[Neuron#neuron{roinsWeights=Neuron#neuron.roinsWeights++[create_weight(Plast,Neuron)||Neuron<-RoLastLayer]}||Neuron<-RoLayers],%%connetto in entrata ricorrente gli strati designati
	{OldLayers++RoConnLayers,RoLastLayer}.

create_neuron(Af,Layer)->
	#neuron{id=?GETID,layer=Layer,af=Af,bias=?RAND,faninsWeights=[],fanouts=[],roinsWeights=[],roouts=[]}.
create_neuron_som(ActivationFunction, X, Y)->
	#neuron_som{id = ?GETID, coordinates = {X, Y}, af = ActivationFunction}.

create_weight(Plast,#sensor{id=Id,vl=Vl})->
	Weight=[?RAND||_<-lists:seq(1,Vl)],
	Modulation=plasticity:get_plasticity(Weight,Plast),
	{Id,Weight,Modulation};
create_weight(Plast,#neuron{id=Id})->
	Weight=[?RAND],
	Modulation=plasticity:get_plasticity(Weight,Plast),
	{Id,Weight,Modulation}.

% som utilities

% modular functions TODO: Vanno implementate

get_elements_filtered(Genotype, Predicate) ->
	{digraph, Vertices, _, _, _} = Genotype,
	Filter = fun(El, Acc) ->
			case Predicate(El) of
				true -> [El | Acc];
				false -> Acc
			end
		end,
	ets:foldl(Filter, [], Vertices).

get_select_on_elements_filtered(Genotype, Predicate, SelectFunction) ->
	{digraph, Vertices, _, _, _} = Genotype,
	Filter = fun(El, Acc) ->
			case Predicate(El) of
				true -> [SelectFunction(El) | Acc];
				false -> Acc
			end
		end,
	ets:foldl(Filter, [], Vertices).
get_element_by_id(Genotype, ElementId) ->
	digraph:vertex(Genotype, ElementId).

get_sensors(Genotype) ->
	Predicate = fun(El) -> is_record(El, sensor) end,
	get_elements_filtered(Genotype, Predicate).

get_sensors_ids(Genotype) ->
	Predicate = fun(El) -> is_record(El, sensor) end,
	Select = fun(#sensor{id = Id}) -> Id end,
	get_select_on_elements_filtered(Genotype, Predicate, Select).
get_actuators(Genotype) ->
	Predicate = fun(El) -> is_record(El, actuator) end,
	get_elements_filtered(Genotype, Predicate).
get_actuators_ids(Genotype) ->
	Predicate = fun(El) -> is_record(El, actuator) end,
	Select = fun(#actuator{id = Id}) -> Id end,
	get_select_on_elements_filtered(Genotype, Predicate, Select).

get_neuron_ids(Genotype, som) ->
	Predicate = fun(El) -> is_record(El, neuron_som) end,
	Select = fun(#neuron_som{id = Id}) -> Id end,
	get_select_on_elements_filtered(Genotype, Predicate, Select);
get_neuron_ids(Genotype, classic) ->
	Predicate = fun(El) -> is_record(El, neuron) end,
	Select = fun(#neuron{id = Id}) -> Id end,
	get_select_on_elements_filtered(Genotype, Predicate, Select).

get_neurons(Genotype, som) ->
	Predicate = fun(El) -> is_record(El, neuron_som) end,
	get_elements_filtered(Genotype, Predicate);
get_neurons(Genotype, classic) ->
	Predicate = fun(El) -> is_record(El, neuron) end,
	get_elements_filtered(Genotype, Predicate).

get_cortex(Genotype) ->
	Predicate = fun(El) -> is_record(El, cortex) end,
	get_elements_filtered(Genotype, Predicate).

get_cortex_id(Genotype) ->
	[Cortex] = get_cortex(Genotype),
	#cortex{id = Id} = Cortex,
	Id.

get_connection(Genotype, IdFrom, IdTo) ->
	EdgeId = {IdFrom, IdTo},
	{EdgeId, IdFrom, IdTo, EdgeInfo} = digraph:edge(Genotype, EdgeId),
	EdgeInfo.


