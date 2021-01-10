-module(genotype_mutator).
-export([mutate/2,mutate_weights/1,mutate_plasticity/1,mutate_bias/1,mutate_af/1,add_neuro_link/1,add_sensor_link/1,
		add_layer_neuron/1,add_neuron/1,clone/1]).
-include("utils.hrl").


switch_ids(#cortex{id=Id,sensorsIds=S,neuronsIds=N,actuatorsIds=A},Mapping)->
	#cortex{id=switch(Id,Mapping),sensorsIds=switch(S,Mapping),neuronsIds=switch(N,Mapping),actuatorsIds=switch(A,Mapping)};
switch_ids(Elements,Mapping)->
	switch_ids(Elements,Mapping,[]).

switch_ids([],_,Acc)->Acc;
switch_ids([Sensor|RestEl],Mapping,Acc)when is_record(Sensor,sensor)->
	#sensor{id=Id,fanouts=Outs}=Sensor,
	SwitchedSensor=Sensor#sensor{id=switch(Id,Mapping),fanouts=switch(Outs,Mapping)},
	switch_ids(RestEl,Mapping,Acc++[SwitchedSensor]);
switch_ids([Neuron|RestEl],Mapping,Acc)when is_record(Neuron,neuron)->
	#neuron{id=Id,faninsWeights=InsWeights,fanouts=Outs,roinsWeights=RoIns,roouts=RoOuts}=Neuron,
	SwitchedNeuron=Neuron#neuron{id=switch(Id,Mapping),faninsWeights=switch(InsWeights,Mapping),fanouts=switch(Outs,Mapping),roinsWeights=switch(RoIns,Mapping),roouts=switch(RoOuts,Mapping)},
	switch_ids(RestEl,Mapping,Acc++[SwitchedNeuron]);
switch_ids([Actuator|RestEl],Mapping,Acc)when is_record(Actuator,actuator)->
	#actuator{id=Id,fanins=Ins,cortexId=CortexId}=Actuator,
	SwitchedActuator=Actuator#actuator{id=switch(Id,Mapping),fanins=switch(Ins,Mapping),cortexId=switch(CortexId,Mapping)},
	switch_ids(RestEl,Mapping,Acc++[SwitchedActuator]).

switch(Id,Mapping)when is_atom(Id)->
	maps:get(Id,Mapping);
switch(Ids,Mapping)when is_list(Ids)->
	switch(Ids,Mapping,[]).

switch([],_,Acc)->Acc;
switch([{Id,V,Mod}|RestIds],Mapping,Acc)->
	switch(RestIds,Mapping,Acc++[{maps:get(Id,Mapping),V,Mod}]);
switch([Id|RestIds],Mapping,Acc)->
	switch(RestIds,Mapping,Acc++[maps:get(Id,Mapping)]).

clone(Genotype)->
	#genotype{sensors=Sensors,neurons=Neurons,actuators=Actuators,cortex=Cortex}=Genotype,
	Ids=genotype:get_ids(Genotype),
	MappingIds=lists:foldl(fun(Id,Map)->maps:merge(Map,#{Id=>?GETID}) end,#{},Ids),%crea un mapping tra vecchi id e nuovi id!
	#genotype{sensors=switch_ids(Sensors,MappingIds),neurons=switch_ids(Neurons,MappingIds),actuators=switch_ids(Actuators,MappingIds),cortex=switch_ids(Cortex,MappingIds)}.

mutate(Genotype,NMutation)->
	Mutators=[?RANDCHOOSE(get_mutators())||_<-lists:seq(1,NMutation+1)],
	{Mutators,lists:foldl(fun(Fun,Geno)->?MODULE:Fun(Geno) end,Genotype,Mutators)}.

get_mutators()->
	[mutate_weights,mutate_bias,mutate_af,add_neuro_link,add_sensor_link,add_layer_neuron,add_neuron,mutate_plasticity].

apply_neuro_mutation(Genotype,Fun)->%mutazione sul singolo elemento
	#genotype{cortex=Cortex,neurons=Neurons}=Genotype,
	NeuronId=?RANDCHOOSE(Cortex#cortex.neuronsIds),
	Pred=fun(Neuron)->Neuron#neuron.id==NeuronId end,
	{[Neuron],OtherNeurons}=lists:partition(Pred,Neurons),
	MutatedNeuron=Fun(Neuron),
	Genotype#genotype{neurons=lists:keysort(3,[MutatedNeuron]++OtherNeurons)}.%mantiene i neuroni in ordine in base al layer!

mutate_weights(Genotype)->
	MutateFun=fun(Neuron)->
    NewIns=[{Id,[neuronPheno:perturbate(W,-?SAT_LIMIT,?SAT_LIMIT)||W<-Weight],M}||{Id,Weight,M}<-Neuron#neuron.faninsWeights],
	NewRoIns=[{Id,[neuronPheno:perturbate(W,-?SAT_LIMIT,?SAT_LIMIT)||W<-Weight],M}||{Id,Weight,M}<-Neuron#neuron.roinsWeights],
	Neuron#neuron{faninsWeights=NewIns,roinsWeights=NewRoIns}
    end,
	apply_neuro_mutation(Genotype,MutateFun).

mutate_plasticity(Genotype)->
	MutateFun=fun(Neuron)->
    NewIns=[{Id,Weight,plasticity:rand_plast(Weight,M)}||{Id,Weight,M}<-Neuron#neuron.faninsWeights],
	NewRoIns=[{Id,Weight,plasticity:rand_plast(Weight,M)}||{Id,Weight,M}<-Neuron#neuron.roinsWeights],
	Neuron#neuron{faninsWeights=NewIns,roinsWeights=NewRoIns}
    end,
	apply_neuro_mutation(Genotype,MutateFun).

mutate_bias(Genotype)->
	MutateFun=fun(Neuron)->
	Neuron#neuron{bias=neuronPheno:perturbate(Neuron#neuron.bias,-?SAT_LIMIT,?SAT_LIMIT)}
	end,
	apply_neuro_mutation(Genotype,MutateFun).

mutate_af(Genotype)->
	MutateFun=fun(Neuron)->
	Af=?RANDCHOOSE(af:get_afs()--[Neuron#neuron.af]),
	Neuron#neuron{af=Af}
	end,
	apply_neuro_mutation(Genotype,MutateFun).

add_neuro_link(Genotype)->%link tra neuroni,o su se stesso oppure tra strati diversi
	#genotype{neurons=Neurons}=Genotype,
	NeuronIn=?RANDCHOOSE(Neurons),
	Pred=fun(N)->N==NeuronIn orelse N#neuron.layer<NeuronIn#neuron.layer orelse N#neuron.layer>NeuronIn#neuron.layer end,
	{Selected,_}=lists:partition(Pred,Neurons),
	NeuronOut=?RANDCHOOSE(Selected),
	RemainNeurons=Neurons--[NeuronIn,NeuronOut],
	case exist_link([NeuronIn,NeuronOut]) of%se non c'è un link allora muto se no non muto
		false->MutatedNeurons=case NeuronIn of
			NeuronOut->% è stato selezionato lo stesso neurone è sicuramente una connessione all'indietro
				NewNeuron=NeuronIn#neuron{roouts=NeuronIn#neuron.roouts++[NeuronIn#neuron.id],roinsWeights=NeuronIn#neuron.roinsWeights++[genotype:create_weight(hebbian,NeuronIn)]},
				[NewNeuron];
			_->
				{NewNeuron1,NewNeuron2}=connect(NeuronIn,NeuronOut),
				[NewNeuron1,NewNeuron2]
			end,
			Genotype#genotype{neurons=lists:keysort(3,MutatedNeurons++RemainNeurons)};
		true->Genotype
	end.

add_sensor_link(Genotype)->
	#genotype{cortex=Cortex,sensors=Sensors,neurons=Neurons}=Genotype,
	{SensorId,NeuronId}={?RANDCHOOSE(Cortex#cortex.sensorsIds),?RANDCHOOSE(Cortex#cortex.neuronsIds)},
	PredSensor=fun(Sensor)->Sensor#sensor.id==SensorId end,
	{[Sensor],OtherSensors}=lists:partition(PredSensor,Sensors),
	PredNeuron=fun(Neuron)->Neuron#neuron.id==NeuronId end,
	{[Neuron],OtherNeurons}=lists:partition(PredNeuron,Neurons),
	case exist_link([Sensor,Neuron]) of
		false->
			{NewSensor,NewNeuron}=connect(Sensor,Neuron),
			Genotype#genotype{sensors=OtherSensors++[NewSensor],neurons=lists:keysort(3,[NewNeuron]++OtherNeurons)};
		true->Genotype
	end.

add_neuron(Genotype)->
	#genotype{sensors=Sensors,neurons=Neurons,cortex=Cortex}=Genotype,
	Layers=genotype:get_layers(Genotype),
	[H|T]=Layers,
	case T of
		[]->Genotype;%se ho un solo layer allora niente
		_->L=?RANDCHOOSE(lists:droplast(Layers)),%ho almeno due strati
			NewNeuron=genotype:create_neuron(sigmund,L),
			case L of
				H->%ho scelto il primo strato
					Sensor=?RANDCHOOSE(Sensors),
					Neuron=?RANDCHOOSE([N||N<-Neurons,N#neuron.layer>L]),%seleziono un neurone da uno strato superiore
					RemainSensors=Sensors--[Sensor],
					RemainNeurons=Neurons--[Neuron],
					{NewSensor,NewNeuronConnIn}=connect(Sensor,NewNeuron),
					{NewConnected,NewOut}=connect(NewNeuronConnIn,Neuron),
					NewCortex=Cortex#cortex{neuronsIds=Cortex#cortex.neuronsIds++[NewNeuron#neuron.id]},
					Genotype#genotype{sensors=RemainSensors++[NewSensor],neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewOut]),cortex=NewCortex};
				_->
					NeuronSup=?RANDCHOOSE([N||N<-Neurons,N#neuron.layer>L]),%seleziono un neurone da uno strato superiore
					NeuronInf=?RANDCHOOSE([N||N<-Neurons,N#neuron.layer<L]),%seleziono un neurone da uno strato inferiore
					RemainNeurons=Neurons--[NeuronSup,NeuronInf],
					{NewInf,NewNeuronConnIn}=connect(NeuronInf,NewNeuron),
					{NewConnected,NewSup}=connect(NewNeuronConnIn,NeuronSup),
					NewCortex=Cortex#cortex{neuronsIds=Cortex#cortex.neuronsIds++[NewNeuron#neuron.id]},
					Genotype#genotype{neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewInf,NewSup]),cortex=NewCortex}
			end
	end.

add_layer_neuron(Genotype)->%aggiunge un nuovo strato tra due strati con un nuovo neurone connesso
	#genotype{sensors=Sensors,neurons=Neurons,cortex=Cortex}=Genotype,
	Layers=genotype:get_layers(Genotype),
	[H|_]=Layers,
	case Layers of
		[1]->%se ho un solo strato,allora connetto tra sensori e neuroni
			Sensor=?RANDCHOOSE(Sensors),
			NewNeuron=genotype:create_neuron(sigmund,0.5),%lo vado a mettere come primo strato
			Neuron=?RANDCHOOSE(Neurons),%seleziono un neurone dal primo strato
			RemainSensors=Sensors--[Sensor],
			RemainNeurons=Neurons--[Neuron],%%cavo i neuroni selezionati
			{NewSensor,NewNeuronConnIn}=connect(Sensor,NewNeuron),
			{NewConnected,NewOut}=connect(NewNeuronConnIn,Neuron),
			NewCortex=Cortex#cortex{neuronsIds=Cortex#cortex.neuronsIds++[NewNeuron#neuron.id]},
			Genotype#genotype{sensors=RemainSensors++[NewSensor],neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewOut]),cortex=NewCortex};
		Layers->%altrimenti
			L=?RANDCHOOSE(lists:droplast(Layers)),%seleziona uno strato interno tranne l'ultimo
			case ?RANDCHOOSE([0,1]) of%lancio una moneta
				0->%lo inserisco prima di L
					case L of
						H->%se L è il primo strato
							Sensor=?RANDCHOOSE(Sensors),
							NewLayer=(L+0)/2,
							NewNeuron=genotype:create_neuron(sigmund,NewLayer),
							SupNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron.layer==L]),%seleziona un neurone casualmente allo strato successivo
							RemainSensors=Sensors--[Sensor],
							RemainNeurons=Neurons--[SupNeuron],
							{NewSensor,NewNeuronConnIn}=connect(Sensor,NewNeuron),
							{NewConnected,NewOut}=connect(NewNeuronConnIn,SupNeuron),
							NewCortex=Cortex#cortex{neuronsIds=Cortex#cortex.neuronsIds++[NewNeuron#neuron.id]},
							Genotype#genotype{sensors=RemainSensors++[NewSensor],neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewOut]),cortex=NewCortex};
						_->
						{InfLayers,_}=lists:splitwith(fun(N)->N<L end,Layers--[L]),%prendi gli strati inferiori
						LastLayer=lists:last(InfLayers),
						NewLayer=(L+LastLayer)/2,%lo sto inserendo prima di L
						NewNeuron=genotype:create_neuron(sigmund,NewLayer),%lo vado a mettere in mezzo tra L e L+1
						InfNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron.layer==LastLayer]),%seleziona un neurone casualmente tra i neurone dello strato PRIMA di L
						SupNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron.layer==L]),%seleziona un neurone casualmente tra i neurone dell strato L
						RemainNeurons=Neurons--[InfNeuron,SupNeuron],%%cavo i neuroni selezionati
						{NewInf,NewNeuronConnIn}=connect(InfNeuron,NewNeuron),
						{NewConnected,NewSup}=connect(NewNeuronConnIn,SupNeuron),
						NewCortex=Cortex#cortex{neuronsIds=Cortex#cortex.neuronsIds++[NewNeuron#neuron.id]},
						Genotype#genotype{neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewInf,NewSup]),cortex=NewCortex}
					end;
				1->%lo inserisco dopo di L
					{_,SupLayers}=lists:splitwith(fun(N)->N<L end,Layers--[L]),%prendi gli strati superiori
					NextLayer=hd(SupLayers),
					NewLayer=(L+NextLayer)/2,%lo sto inserendo dopo di L
					NewNeuron=genotype:create_neuron(sigmund,NewLayer),%lo vado a mettere in mezzo tra L e L+1
					InfNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron.layer==L]),%seleziona un neurone casualmente tra i neurone dell strato L
					SupNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron.layer==NextLayer]),%seleziona un neurone casualmente tra i neurone dello strato SUCCESSIVO ad L
					RemainNeurons=Neurons--[InfNeuron,SupNeuron],%%cavo i neuroni selezionati
					{NewInf,NewNeuronConnIn}=connect(InfNeuron,NewNeuron),
					{NewConnected,NewSup}=connect(NewNeuronConnIn,SupNeuron),
					NewCortex=Cortex#cortex{neuronsIds=Cortex#cortex.neuronsIds++[NewNeuron#neuron.id]},
					Genotype#genotype{neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewInf,NewSup]),cortex=NewCortex}
			end
	end.	

connect(N1,N2)when N1#neuron.layer<N2#neuron.layer->%è una connessione in avanti
	NewN1=N1#neuron{fanouts=N1#neuron.fanouts++[N2#neuron.id]},
	NewN2=N2#neuron{faninsWeights=N2#neuron.faninsWeights++[genotype:create_weight(hebbian,N1)]},
	{NewN1,NewN2};
connect(N1,N2)when N1#neuron.layer>=N2#neuron.layer->%è una connessione all'indietro
	NewN1=N1#neuron{roouts=N1#neuron.roouts++[N2#neuron.id]},
	NewN2=N2#neuron{roinsWeights=N2#neuron.roinsWeights++[genotype:create_weight(hebbian,N1)]},
	{NewN1,NewN2};
connect(Sensor,Neuron) when is_record(Sensor,sensor)->
	NewSensor=Sensor#sensor{fanouts=Sensor#sensor.fanouts++[Neuron#neuron.id]},
	NewNeuron=Neuron#neuron{faninsWeights=Neuron#neuron.faninsWeights++[genotype:create_weight(hebbian,Sensor)]},
	{NewSensor,NewNeuron}.


exist_link([#neuron{id=Id,roouts=RoOuts}])->
	lists:member(Id,RoOuts);
exist_link([#sensor{fanouts=Outs},#neuron{id=Id}])->
	lists:member(Id,Outs);
exist_link([#neuron{layer=L1,fanouts=Outs},#neuron{id=Id2,layer=L2}])when L1<L2->
	lists:member(Id2,Outs);
exist_link([#neuron{id=Id1,layer=L1},#neuron{layer=L2,roouts=RoOuts}])when L1>=L2->
	lists:member(Id1,RoOuts).
