-module(genotype).
-export([create_NN/4]).
-export([get_cortex_id/1,get_neurons_ids/1,get_sensors_ids/1,get_actuators_ids/1,get_ids/1]).
-export([create_neuron/2,create_weight/2,get_layers/1,get_geno_spec/1]).
-define(LAYER(Neurons),(hd(Neurons))#neuron.layer).
-define(NIDS(Neurons),[NeuronId||#neuron{id=NeuronId}<-Neurons]).%%get neurons ids from a list of neurons
-include("utils.hrl").

get_cortex_id(Genotype)->#genotype{cortex=Cortex}=Genotype,Cortex#cortex.id.
get_neurons_ids(Genotype)->#genotype{neurons=Neurons}=Genotype,[Neuron#neuron.id||Neuron<-Neurons].
get_sensors_ids(Genotype)->#genotype{sensors=Sensors}=Genotype,[Sensor#sensor.id||Sensor<-Sensors].
get_actuators_ids(Genotype)->#genotype{actuators=Actuators}=Genotype,[Act#actuator.id||Act<-Actuators].
get_ids(Genotype)->get_sensors_ids(Genotype)++get_neurons_ids(Genotype)++get_actuators_ids(Genotype)++[get_cortex_id(Genotype)].
get_geno_spec(Genotype)->
	#genotype{sensors=[S],actuators=[A]}=Genotype,
	#sensor{vl=SVl,fit_directives=SFit,real_directives=SReal}=S,
	#actuator{vl=AVl,fit_directives=AFit,real_directives=AReal}=A,
	{{SVl,SFit,SReal},{AVl,AFit,AReal},lists:droplast(get_layers(Genotype))}.

get_layers(Genotype)->
	#genotype{neurons=Net}=Genotype,
	Layer=fun(#neuron{layer=L},Layers)->case L>lists:last(Layers) of true->Layers++[L];false->Layers end end,
	erlang:tl(lists:foldl(Layer,[0],Net)).

create_NN({rnn,Af,Plast},SensorSpec,ActuatorSpec,HiddenLayerDensity)->
	create_NN({{rnn,0},Af,Plast},SensorSpec,ActuatorSpec,HiddenLayerDensity);
create_NN(Constraint,{SensorVl,SFitDirectives,SRealDirectives},{ActuatorVl,AFitDirectives,ARealDirectives},HiddenLayerDensity) ->
	LayerDensity=HiddenLayerDensity++[ActuatorVl],
	Sensor=#sensor{id=?GETID,vl=SensorVl,fit_directives=SFitDirectives,real_directives=SRealDirectives},
	Actuator=#actuator{id=?GETID,vl=ActuatorVl,fit_directives=AFitDirectives,real_directives=ARealDirectives},
	{ConnSensor,Net,ConnActuator}=create_neuro_net(Constraint,{Sensor,[],Actuator},LayerDensity,1),
	SensorsIds=[SId||#sensor{id=SId}<-[Sensor]],
	NetIds=[NId||#neuron{id=NId}<-Net],
	ActuatorsIds=[AId||#actuator{id=AId}<-[Actuator]],
	Cortex=#cortex{id=?GETID,sensorsIds=SensorsIds,neuronsIds=NetIds,actuatorsIds=ActuatorsIds},
	NewActuator=ConnActuator#actuator{cortexId=Cortex#cortex.id},
	#genotype{sensors=[ConnSensor],neurons=Net,actuators=[NewActuator],cortex=Cortex}.

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

create_weight(Plast,#sensor{id=Id,vl=Vl})->
	Weight=[?RAND||_<-lists:seq(1,Vl)],
	Modulation=plasticity:get_plasticity(Weight,Plast),
	{Id,Weight,Modulation};
create_weight(Plast,#neuron{id=Id})->
	Weight=[?RAND],
	Modulation=plasticity:get_plasticity(Weight,Plast),
	{Id,Weight,Modulation}.
