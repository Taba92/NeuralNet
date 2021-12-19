-module(genotype_mutator).
-export([mutate/3,mutate_weights/2,mutate_plasticity/2,mutate_bias/2,mutate_af/2,add_neuro_link/2,add_sensor_link/2,
		add_layer_neuron/2,add_neuron/2,clone/1]).
-include("utils.hrl").
-include("genotype.hrl").
-include("phenotype.hrl").


switch_ids(#cortex_phenotype{id=Id,sensorsIds=S,neuronsIds=N,actuatorsIds=A},Mapping)->
	#cortex_phenotype{id=switch(Id,Mapping),sensorsIds=switch(S,Mapping),neuronsIds=switch(N,Mapping),actuatorsIds=switch(A,Mapping)};
switch_ids(Elements,Mapping)->
	switch_ids(Elements,Mapping,[]).

switch_ids([],_,Acc)->Acc;
switch_ids([Sensor|RestEl],Mapping,Acc)when is_record(Sensor,sensor_phenotype)->
	#sensor_phenotype{id=Id,fanouts=Outs}=Sensor,
	SwitchedSensor=Sensor#sensor_phenotype{id=switch(Id,Mapping),fanouts=switch(Outs,Mapping)},
	switch_ids(RestEl,Mapping,Acc++[SwitchedSensor]);
switch_ids([Neuron|RestEl],Mapping,Acc)when is_record(Neuron,neuron_classic_phenotype)->
	#neuron_classic_phenotype{id=Id,faninsWeights=InsWeights,fanouts=Outs,roinsWeights=RoIns,roouts=RoOuts}=Neuron,
	SwitchedNeuron=Neuron#neuron_classic_phenotype{id=switch(Id,Mapping),faninsWeights=switch(InsWeights,Mapping),fanouts=switch(Outs,Mapping),roinsWeights=switch(RoIns,Mapping),roouts=switch(RoOuts,Mapping)},
	switch_ids(RestEl,Mapping,Acc++[SwitchedNeuron]);
switch_ids([Actuator|RestEl],Mapping,Acc)when is_record(Actuator,actuator_phenotype)->
	#actuator_phenotype{id=Id,fanins=Ins,cortex_id=CortexId}=Actuator,
	SwitchedActuator=Actuator#actuator_phenotype{id=switch(Id,Mapping),fanins=switch(Ins,Mapping),cortex_id=switch(CortexId,Mapping)},
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
	Genotype#genotype{sensors=switch_ids(Sensors,MappingIds),neurons=switch_ids(Neurons,MappingIds),actuators=switch_ids(Actuators,MappingIds),cortex=switch_ids(Cortex,MappingIds)}.

mutate(Genotype,NMutation,Constraint)->
	{MutatorsConstraint,SpecificsConstraint}=parse_constraint(Constraint),
	Mutators=[?RANDCHOOSE(MutatorsConstraint)||_<-lists:seq(1,NMutation)],
	lists:foldl(fun(Fun,Geno)->?MODULE:Fun(Geno,SpecificsConstraint) end, {Genotype, []}, Mutators).


parse_constraint(none) ->
	{get_mutators(), [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic()}, {plast, plasticity:all()}]};
parse_constraint(#{mutators := none, af := none, plast := none}) -> 
	parse_constraint(none);
parse_constraint(#{mutators := none, af := Afs , plast := Plasts}) when is_list(Afs),is_list(Plasts) ->
	{get_mutators(), [{af, Afs}, {plast, Plasts}]};
parse_constraint(#{mutators := none, af := Afs, plast := none}) when is_list(Afs) ->
	{get_mutators(), [{af, Afs}, {plast, plasticity:all()}]};
parse_constraint(#{mutators := none, af := none, plast := Plasts}) when is_list(Plasts) ->
	{get_mutators(), [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic()}, {plast, Plasts}]};
parse_constraint(#{mutators := Mutators, af := none, plast := Plasts}) when is_list(Mutators),is_list(Plasts) ->
	{Mutators, [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic()}, {plast, Plasts}]};
parse_constraint(#{mutators := Mutators, af := Afs, plast := none}) when is_list(Mutators),is_list(Afs) ->
	{Mutators, [{af, Afs}, {plast, plasticity:all()}]};
parse_constraint(#{mutators := Mutators, af := none, plast := none}) when is_list(Mutators) ->
	{Mutators, [{af, ?ACTIVATION_FUNCTION_MODULE:all_activation_functions_classic()}, {plast, plasticity:all()}]};
parse_constraint(#{mutators := Mutators, af := Afs, plast := Plasts}) when is_list(Mutators),is_list(Afs),is_list(Plasts) ->
	{Mutators, [{af, Afs}, {plast, Plasts}]}.

get_mutators()->
	[mutate_weights,mutate_bias,mutate_af,add_neuro_link,add_sensor_link,add_layer_neuron,add_neuron,mutate_plasticity].

mutate_weights({Genotype, Mutations}, _)->
	#genotype{neurons=Neurons}=Genotype,
	Neuron=?RANDCHOOSE(Neurons),
	RemainNeurons=Neurons--[Neuron],
	OldInsWeights = Neuron#neuron_classic_phenotype.faninsWeights,
	OldRoInsWeights = Neuron#neuron_classic_phenotype.roinsWeights,
    NewIns=[{Id,[utils:perturbate(W)||W<-Weight],M}||{Id,Weight,M}<-Neuron#neuron_classic_phenotype.faninsWeights],
	NewRoIns=[{Id,[utils:perturbate(W)||W<-Weight],M}||{Id,Weight,M}<-Neuron#neuron_classic_phenotype.roinsWeights],
	MutatedNeuron=Neuron#neuron_classic_phenotype{faninsWeights=NewIns,roinsWeights=NewRoIns},
	Mutation = #{type => weights, neuron_id => Neuron#neuron_classic_phenotype.id, old_weights => {OldInsWeights, OldRoInsWeights}, new_weights => {NewIns, NewRoIns}},
	{Genotype#genotype{neurons=lists:keysort(3,[MutatedNeuron]++RemainNeurons)}, Mutations ++ [Mutation]}.

mutate_plasticity({Genotype, Mutations}, Constraint)->
	{plast,Plasts}=lists:keyfind(plast,1,Constraint),
	#genotype{neurons=Neurons}=Genotype,
	Neuron=?RANDCHOOSE(Neurons),
	RemainNeurons=Neurons--[Neuron],
	OldInsWeights = Neuron#neuron_classic_phenotype.faninsWeights,
	OldRoInsWeights = Neuron#neuron_classic_phenotype.roinsWeights,
    NewIns=[{Id,Weight,plasticity:get_rand_plast(Weight,Plasts)}||{Id,Weight,_}<-Neuron#neuron_classic_phenotype.faninsWeights],
	NewRoIns=[{Id,Weight,plasticity:get_rand_plast(Weight,Plasts)}||{Id,Weight,_}<-Neuron#neuron_classic_phenotype.roinsWeights],
    MutatedNeuron=Neuron#neuron_classic_phenotype{faninsWeights=NewIns,roinsWeights=NewRoIns},
	Mutation = #{type => plasticity, neuron_id => Neuron#neuron_classic_phenotype.id, old_weights => {OldInsWeights, OldRoInsWeights}, new_weights => {NewIns, NewRoIns}},
	{Genotype#genotype{neurons=lists:keysort(3,[MutatedNeuron]++RemainNeurons)}, Mutations ++ [Mutation]}.

mutate_bias({Genotype, Mutations}, _)->
	#genotype{neurons=Neurons}=Genotype,
	Neuron=?RANDCHOOSE(Neurons),
	RemainNeurons=Neurons--[Neuron],
	OldBias = Neuron#neuron_classic_phenotype.bias,
	MutatedNeuron=Neuron#neuron_classic_phenotype{bias=utils:perturbate(Neuron#neuron_classic_phenotype.bias)},
	NewBias = MutatedNeuron#neuron_classic_phenotype.bias,
	Mutation = #{type => bias, neuron_id => Neuron#neuron_classic_phenotype.id, old_bias => OldBias, new_bias => NewBias},
	{Genotype#genotype{neurons=lists:keysort(3,[MutatedNeuron]++RemainNeurons)}, Mutations ++ [Mutation]}.

mutate_af({Genotype, Mutations}, Constraint)->
	{af,Afs}=lists:keyfind(af,1,Constraint),
	#genotype{neurons=Neurons}=Genotype,
	Neuron=?RANDCHOOSE(Neurons),
	RemainNeurons=Neurons--[Neuron],
	OldAf = Neuron#neuron_classic_phenotype.af,
	NewAf=?RANDCHOOSE(Afs),
	MutatedNeuron=Neuron#neuron_classic_phenotype{af=NewAf},
	Mutation = #{type => activation_function, neuron_id => Neuron#neuron_classic_phenotype.id, old_af => OldAf, new_af => NewAf},
	{Genotype#genotype{neurons=lists:keysort(3,[MutatedNeuron]++RemainNeurons)}, Mutations ++ [Mutation]}.

add_neuro_link({Genotype, Mutations}, Constraint)->%link tra neuroni,o su se stesso oppure con un altro neurone in qualsiasi strato.
	{plast,Plasts}=lists:keyfind(plast,1,Constraint),
	#genotype{neurons=Neurons}=Genotype,
	NeuronIn=?RANDCHOOSE(Neurons),
	NeuronOut=?RANDCHOOSE(Neurons),
	RemainNeurons=Neurons--[NeuronIn,NeuronOut],
	case exist_link([NeuronIn,NeuronOut]) of%se non c'è un link allora muto se no non muto
		false->MutatedNeurons=case NeuronIn of
			NeuronOut->% è stato selezionato lo stesso neurone è sicuramente una connessione all'indietro
				NewNeuron=NeuronIn#neuron_classic_phenotype{roouts=NeuronIn#neuron_classic_phenotype.roouts++[NeuronIn#neuron_classic_phenotype.id],roinsWeights=NeuronIn#neuron_classic_phenotype.roinsWeights++[genotype:create_weight(hebbian,NeuronIn)]},
				[NewNeuron];
			_->
				{NewNeuron1,NewNeuron2}=connect(NeuronIn,NeuronOut,Plasts),
				[NewNeuron1,NewNeuron2]
			end,
			Mutation = #{type => neuro_link, link_from => NeuronIn#neuron_classic_phenotype.id, link_to => NeuronOut#neuron_classic_phenotype.id},
			{Genotype#genotype{neurons=lists:keysort(3,MutatedNeurons++RemainNeurons)}, Mutations ++ [Mutation]};
		true->{Genotype, Mutations}
	end.

add_sensor_link({Genotype, Mutations}, Constraint)->
	{plast,Plasts}=lists:keyfind(plast,1,Constraint),
	#genotype{cortex=Cortex,sensors=Sensors,neurons=Neurons}=Genotype,
	{SensorId,NeuronId}={?RANDCHOOSE(Cortex#cortex_phenotype.sensorsIds),?RANDCHOOSE(Cortex#cortex_phenotype.neuronsIds)},
	PredSensor=fun(Sensor)->Sensor#sensor_phenotype.id==SensorId end,
	{[Sensor],OtherSensors}=lists:partition(PredSensor,Sensors),
	PredNeuron=fun(Neuron)->Neuron#neuron_classic_phenotype.id==NeuronId end,
	{[Neuron],OtherNeurons}=lists:partition(PredNeuron,Neurons),
	case exist_link([Sensor,Neuron]) of
		false->
			{NewSensor,NewNeuron}=connect(Sensor,Neuron,Plasts),
			Mutation = #{type => sensor_link, link_from => Sensor#sensor_phenotype.id, link_to => Neuron#neuron_classic_phenotype.id},
			{Genotype#genotype{sensors=OtherSensors++[NewSensor],neurons=lists:keysort(3,[NewNeuron]++OtherNeurons)}, Mutations ++ [Mutation]};
		true->{Genotype, Mutations}
	end.

add_neuron({Genotype, Mutations}, Constraint)->
	{plast,Plasts}=lists:keyfind(plast,1,Constraint),
	{af,Afs}=lists:keyfind(af,1,Constraint),
	#genotype{sensors=Sensors,neurons=Neurons,cortex=Cortex}=Genotype,
	Layers=genotype:get_layers(Genotype),
	[H|T]=Layers,
	case T of
		[]->Genotype;%se ho un solo layer allora niente
		_->L=?RANDCHOOSE(lists:droplast(Layers)),%ho almeno due strati
			NewNeuron=genotype:create_neuron(?RANDCHOOSE(Afs),L),
			case L of
				H->%ho scelto il primo strato
					Sensor=?RANDCHOOSE(Sensors),
					Neuron=?RANDCHOOSE([N||N<-Neurons,N#neuron_classic_phenotype.layer>L]),%seleziono un neurone da uno strato superiore
					RemainSensors=Sensors--[Sensor],
					RemainNeurons=Neurons--[Neuron],
					{NewSensor,NewNeuronConnIn}=connect(Sensor,NewNeuron,Plasts),
					{NewConnected,NewOut}=connect(NewNeuronConnIn,Neuron,Plasts),
					NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_phenotype.id]},
					Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_phenotype.id, connected_with => {Sensor#sensor_phenotype.id, Neuron#neuron_classic_phenotype.id}},
					{Genotype#genotype{sensors=RemainSensors++[NewSensor],neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewOut]),cortex=NewCortex}, Mutations ++ [Mutation]};
				_->
					NeuronSup=?RANDCHOOSE([N||N<-Neurons,N#neuron_classic_phenotype.layer>L]),%seleziono un neurone da uno strato superiore
					NeuronInf=?RANDCHOOSE([N||N<-Neurons,N#neuron_classic_phenotype.layer<L]),%seleziono un neurone da uno strato inferiore
					RemainNeurons=Neurons--[NeuronSup,NeuronInf],
					{NewInf,NewNeuronConnIn}=connect(NeuronInf,NewNeuron,Plasts),
					{NewConnected,NewSup}=connect(NewNeuronConnIn,NeuronSup,Plasts),
					NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_phenotype.id]},
					Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_phenotype.id, connected_with => {NeuronInf#neuron_classic_phenotype.id, NeuronSup#neuron_classic_phenotype.id}},
					{Genotype#genotype{neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewInf,NewSup]),cortex=NewCortex}, Mutations ++ [Mutation]}
			end
	end.

add_layer_neuron({Genotype, Mutations}, Constraint)->%aggiunge un nuovo strato tra due strati con un nuovo neurone connesso
	{plast,Plasts}=lists:keyfind(plast,1,Constraint),
	{af,Afs}=lists:keyfind(af,1,Constraint),
	#genotype{sensors=Sensors,neurons=Neurons,cortex=Cortex}=Genotype,
	Layers=genotype:get_layers(Genotype),
	[H|_]=Layers,
	case Layers of
		[1]->%se ho un solo strato,allora connetto tra sensori e neuroni
			Sensor=?RANDCHOOSE(Sensors),
			NewNeuron=genotype:create_neuron(?RANDCHOOSE(Afs),0.5),%lo vado a mettere come primo strato
			Neuron=?RANDCHOOSE(Neurons),%seleziono un neurone dal primo strato
			RemainSensors=Sensors--[Sensor],
			RemainNeurons=Neurons--[Neuron],%%cavo i neuroni selezionati
			{NewSensor,NewNeuronConnIn}=connect(Sensor,NewNeuron,Plasts),
			{NewConnected,NewOut}=connect(NewNeuronConnIn,Neuron,Plasts),
			NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_phenotype.id]},
			Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_phenotype.id, connected_with => {Sensor#sensor_phenotype.id, Neuron#neuron_classic_phenotype.id}},
			{Genotype#genotype{sensors=RemainSensors++[NewSensor],neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewOut]),cortex=NewCortex}, Mutations ++ [Mutation]};
		Layers->%altrimenti
			L=?RANDCHOOSE(lists:droplast(Layers)),%seleziona uno strato interno tranne l'ultimo
			case ?RANDCHOOSE([0,1]) of%lancio una moneta
				0->%lo inserisco prima di L
					case L of
						H->%se L è il primo strato
							Sensor=?RANDCHOOSE(Sensors),
							NewLayer=(L+0)/2,
							NewNeuron=genotype:create_neuron(?RANDCHOOSE(Afs),NewLayer),
							SupNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron_classic_phenotype.layer==L]),%seleziona un neurone casualmente allo strato successivo
							RemainSensors=Sensors--[Sensor],
							RemainNeurons=Neurons--[SupNeuron],
							{NewSensor,NewNeuronConnIn}=connect(Sensor,NewNeuron,Plasts),
							{NewConnected,NewOut}=connect(NewNeuronConnIn,SupNeuron,Plasts),
							NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_phenotype.id]},
							Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_genotype.id, connected_with => {Sensor#sensor_phenotype.id, SupNeuron#neuron_classic_genotype.id}},
							{Genotype#genotype{sensors=RemainSensors++[NewSensor],neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewOut]),cortex=NewCortex}, Mutations ++ [Mutation]};
						_->
						{InfLayers,_}=lists:splitwith(fun(N)->N<L end,Layers--[L]),%prendi gli strati inferiori
						LastLayer=lists:last(InfLayers),
						NewLayer=(L+LastLayer)/2,%lo sto inserendo prima di L
						NewNeuron=genotype:create_neuron(?RANDCHOOSE(Afs),NewLayer),%lo vado a mettere in mezzo tra L e L+1
						InfNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron_classic_genotype.layer==LastLayer]),%seleziona un neurone casualmente tra i neurone dello strato PRIMA di L
						SupNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron_classic_genotype.layer==L]),%seleziona un neurone casualmente tra i neurone dell strato L
						RemainNeurons=Neurons--[InfNeuron,SupNeuron],%%cavo i neuroni selezionati
						{NewInf,NewNeuronConnIn}=connect(InfNeuron,NewNeuron,Plasts),
						{NewConnected,NewSup}=connect(NewNeuronConnIn,SupNeuron,Plasts),
						NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_genotype.id]},
						Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_genotype.id, connected_with => {InfNeuron#neuron_classic_genotype.id, SupNeuron#neuron_classic_genotype.id}},
						{Genotype#genotype{neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewInf,NewSup]),cortex=NewCortex}, Mutations ++ [Mutation]}
					end;
				1->%lo inserisco dopo di L
					{_,SupLayers}=lists:splitwith(fun(N)->N<L end,Layers--[L]),%prendi gli strati superiori
					NextLayer=hd(SupLayers),
					NewLayer=(L+NextLayer)/2,%lo sto inserendo dopo di L
					NewNeuron=genotype:create_neuron(?RANDCHOOSE(Afs),NewLayer),%lo vado a mettere in mezzo tra L e L+1
					InfNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron_classic_genotype.layer==L]),%seleziona un neurone casualmente tra i neurone dell strato L
					SupNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron_classic_genotype.layer==NextLayer]),%seleziona un neurone casualmente tra i neurone dello strato SUCCESSIVO ad L
					RemainNeurons=Neurons--[InfNeuron,SupNeuron],%%cavo i neuroni selezionati
					{NewInf,NewNeuronConnIn}=connect(InfNeuron,NewNeuron,Plasts),
					{NewConnected,NewSup}=connect(NewNeuronConnIn,SupNeuron,Plasts),
					NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_genotype.id]},
					Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_genotype.id, connected_with => {InfNeuron#neuron_classic_genotype.id, SupNeuron#neuron_classic_genotype.id}},
					{Genotype#genotype{neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewInf,NewSup]),cortex=NewCortex}, Mutations ++ [Mutation]}
			end
	end.	

connect(N1,N2,Plasts)when N1#neuron_classic_genotype.layer<N2#neuron_classic_genotype.layer->%è una connessione in avanti
	Plast=plasticity:random_plasticity(Plasts),
	NewN1=N1#neuron_classic_phenotype{fanouts=N1#neuron_classic_phenotype.fanouts++[N2#neuron_classic_genotype.id]},
	%NewN2=N2#neuron{faninsWeights=N2#neuron.faninsWeights++[genotype:create_weight(Plast,N1)]},
	NewN2 = null,
	{NewN1,NewN2};
connect(N1,N2,Plasts)when N1#neuron_classic_genotype.layer>=N2#neuron_classic_genotype.layer->%è una connessione all'indietro
	Plast=plasticity:random_plasticity(Plasts),
	NewN1=N1#neuron_classic_phenotype{roouts=N1#neuron_classic_phenotype.roouts++[N2#neuron_classic_genotype.id]},
	NewN2=none, %N2#neuron{roinsWeights=N2#neuron.roinsWeights++[genotype:create_weight(Plast,N1)]},
	{NewN1,NewN2};
connect(Sensor,Neuron,Plasts) when is_record(Sensor,sensor_genotype)->
	Plast=plasticity:random_plasticity(Plasts),
	NewSensor=Sensor#sensor_phenotype{fanouts=Sensor#sensor_phenotype.fanouts++[Neuron#neuron_classic_genotype.id]},
	NewNeuron=none, %Neuron#neuron{faninsWeights=Neuron#neuron.faninsWeights++[genotype:create_weight(Plast,Sensor)]},
	{NewSensor,NewNeuron}.


exist_link([#neuron_classic_phenotype{id=Id,roouts=RoOuts}])->
	lists:member(Id,RoOuts);
exist_link([#sensor_phenotype{fanouts=Outs},#neuron_classic_phenotype{id=Id}])->
	lists:member(Id,Outs);
exist_link([#neuron_classic_phenotype{layer=L1,fanouts=Outs},#neuron_classic_phenotype{id=Id2,layer=L2}])when L1<L2->
	lists:member(Id2,Outs);
exist_link([#neuron_classic_phenotype{id=Id1,layer=L1},#neuron_classic_phenotype{layer=L2,roouts=RoOuts}])when L1>=L2->
	lists:member(Id1,RoOuts).
