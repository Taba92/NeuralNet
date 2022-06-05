-module(genotype_mutator).
-export([mutate/3,mutate_weights/2,mutate_plasticity/2,mutate_bias/2,mutate_af/2,add_neuro_link/2,add_sensor_link/2,
		add_layer_neuron/2,add_neuron/2, clone/1]).
-include("utils.hrl").
-include("genotype.hrl").
-include("phenotype.hrl").

clone(Genotype) ->
	#genotype{network_type = Networktype, network = Network} = Genotype,
	%1) Initialize a new empty genotype
	NewGenotype = genotype:new(NetworkType),
	%2) Clone elements in the new genotype and create mapping between old ids and new ids
	CloneNodeFun = fun(ElementId, Acc) ->
						ElementGenotype = genotype:get_element_by_id(Genotype, ElementId),
						{NewElementId, NewElementGenotype} = clone_element(ElementGenotype),
						genotype:add_element_with_genotype(NewGenotype, NewElementGenotype),
						[{ElementId, NewElementId} | Acc]
				   end,
	MappingIds = lists:foldl(CloneNodeFun, [], genotype:get_elements_ids(Genotype)),
	%3) Clone synapses in the new genotype
	CloneSynapseFun = fun({IdFrom, IdTo}) ->
						SynapseGenotype = genotype:get_synapses(Genotype, IdFrom, IdTo),
						{_, NewSynapseGenotype} = clone_synapse(SynapseGenotype, MappingIds),
						genotype:add_element_with_genotype(NewGenotype, NewSynapseGenotype)
					  end,
	lists:map(CloneSynapseFun, genotype:get_synapses_ids(Genotype)),
	NewGenotype.

clone_element(ElementGenotype) when is_record(ElementGenotype, cortex_genotype) ->
	NewId = ?GETID,
	{NewId, ElementGenotype#cortex_genotype{id = NewId}};
clone_element(ElementGenotype) when is_record(ElementGenotype, sensor_genotype) ->
	NewId = ?GETID,
	{NewId, ElementGenotype#sensor_genotype{id = NewId}};
clone_element(ElementGenotype) when is_record(ElementGenotype, actuator_genotype) ->
	NewId = ?GETID,
	{NewId, ElementGenotype#actuator_genotype{id = NewId}};
clone_element(ElementGenotype) when is_record(ElementGenotype, neuron_classic_genotype) ->
	NewId = ?GETID,
	{NewId, ElementGenotype#neuron_classic_genotype{id = NewId}};
clone_element(ElementGenotype) when is_record(ElementGenotype, neuron_som_genotype) ->
	NewId = ?GETID,
	{NewId, ElementGenotype#neuron_som_genotype{id = NewId}}.

clone_synapse(SynapseGenotype, MappingIds) when is_record(SynapseGenotype, synapses) ->
	#synapses_genotype{id_from = IdFrom, id_to = IdTo} = SynapseGenotype,
	%1) Extract new nodes ids of the edge
	{IdFrom, NewIdFrom} = lists:keyfind(IdFrom, 1, MappingIds),
	{IdTo, NewIdTo} = lists:keyfind(IdTo, 1, MappingIds),
	SynapseGenotype#synapses_genotype{id_from = NewIdFrom, id_to = NewIdTo}.

mutate(Genotype, NMutation, Constraint)->
	{MutatorsConstraint, SpecificsConstraint} = parse_constraint(Constraint),
	Mutators = [?RANDCHOOSE(MutatorsConstraint) || _ <- lists:seq(1, NMutation)],
	lists:foldl(fun(Fun, Geno) -> ?MODULE:Fun(Geno, SpecificsConstraint) end, {Genotype, []}, Mutators).


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
	[mutate_weights, mutate_bias, mutate_af, add_neuro_link, add_sensor_link, add_layer_neuron, add_neuron, mutate_plasticity].

mutate_weights({Genotype, Mutations}, _)->
	%1) Select a random edge id
	{IdFrom, IdTo} = ?RANDCHOOSE(genotype:get_synapses_ids(Genotype)),
	SynapseGenotype = genotype:get_synapses(Genotype, IdFrom, IdTo),
	%2) Perturb the weight
	NewWeight = [?NN_SERVICE_MODULE:perturbate(X) || X <- SynapseGenotype#synapses.weight],
	NewSynapseGenotype = SynapseGenotype#synapses{weight = NewWeight},
	%3) Update the synapse with the new weight
	genotype:update_synapse_genotype(Genotype, IdFrom, IdTo, NewSynapseGenotype),
	Mutation = #{type => weights, synapse_id => {IdFrom, IdTo}, old_weights => SynapseGenotype#synapses.weight, new_weights => NewWeight},
	{Genotype, Mutations ++ [Mutation]}.

mutate_plasticity({Genotype, Mutations}, Constraint)->
	%{plast,Plasts}=lists:keyfind(plast,1,Constraint),
	%#genotype{neurons=Neurons}=Genotype,
	%Neuron=?RANDCHOOSE(Neurons),
	%RemainNeurons=Neurons--[Neuron],
	%OldInsWeights = Neuron#neuron_classic_phenotype.faninsWeights,
	%OldRoInsWeights = Neuron#neuron_classic_phenotype.roinsWeights,
    %NewIns=[{Id,Weight,plasticity:get_rand_plast(Weight,Plasts)}||{Id,Weight,_}<-Neuron#neuron_classic_phenotype.faninsWeights],
	%NewRoIns=[{Id,Weight,plasticity:get_rand_plast(Weight,Plasts)}||{Id,Weight,_}<-Neuron#neuron_classic_phenotype.roinsWeights],
    %MutatedNeuron=Neuron#neuron_classic_phenotype{faninsWeights=NewIns,roinsWeights=NewRoIns},
	%Mutation = #{type => plasticity, neuron_id => Neuron#neuron_classic_phenotype.id, old_weights => {OldInsWeights, OldRoInsWeights}, new_weights => {NewIns, NewRoIns}},
	%{Genotype#genotype{neurons=lists:keysort(3,[MutatedNeuron]++RemainNeurons)}, Mutations ++ [Mutation]}.
	ok.

mutate_bias({Genotype, Mutations}, _)->
	%#genotype{neurons=Neurons}=Genotype,
	%Neuron=?RANDCHOOSE(Neurons),
	%RemainNeurons=Neurons--[Neuron],
	%OldBias = Neuron#neuron_classic_phenotype.bias,
	%MutatedNeuron=Neuron#neuron_classic_phenotype{bias=utils:perturbate(Neuron#neuron_classic_phenotype.bias)},
	%NewBias = MutatedNeuron#neuron_classic_phenotype.bias,
	%utation = #{type => bias, neuron_id => Neuron#neuron_classic_phenotype.id, old_bias => OldBias, new_bias => NewBias},
	%{Genotype#genotype{neurons=lists:keysort(3,[MutatedNeuron]++RemainNeurons)}, Mutations ++ [Mutation]}.
	ok.

mutate_af({Genotype, Mutations}, Constraint)->
	%{af,Afs}=lists:keyfind(af,1,Constraint),
	%#genotype{neurons=Neurons}=Genotype,
	%Neuron=?RANDCHOOSE(Neurons),
	%RemainNeurons=Neurons--[Neuron],
	%OldAf = Neuron#neuron_classic_phenotype.af,
	%NewAf=?RANDCHOOSE(Afs),
	%MutatedNeuron=Neuron#neuron_classic_phenotype{af=NewAf},
	%Mutation = #{type => activation_function, neuron_id => Neuron#neuron_classic_phenotype.id, old_af => OldAf, new_af => NewAf},
	%{Genotype#genotype{neurons=lists:keysort(3,[MutatedNeuron]++RemainNeurons)}, Mutations ++ [Mutation]}.
	ok.

add_neuro_link({Genotype, Mutations}, Constraint)->%link tra neuroni,o su se stesso oppure con un altro neurone in qualsiasi strato.
	%{plast,Plasts}=lists:keyfind(plast,1,Constraint),
	%#genotype{neurons=Neurons}=Genotype,
	%NeuronIn=?RANDCHOOSE(Neurons),
	%NeuronOut=?RANDCHOOSE(Neurons),
	%RemainNeurons=Neurons--[NeuronIn,NeuronOut],
	%case exist_link([NeuronIn,NeuronOut]) of%se non c'è un link allora muto se no non muto
	%	false->MutatedNeurons=case NeuronIn of
	%		NeuronOut->% è stato selezionato lo stesso neurone è sicuramente una connessione all'indietro
	%			NewNeuron=NeuronIn#neuron_classic_phenotype{roouts=NeuronIn#neuron_classic_phenotype.roouts++[NeuronIn#neuron_classic_phenotype.id],roinsWeights=NeuronIn#neuron_classic_phenotype.roinsWeights++[genotype:create_weight(hebbian,NeuronIn)]},
	%			[NewNeuron];
	%		_->
	%			{NewNeuron1,NewNeuron2}=connect(NeuronIn,NeuronOut,Plasts),
	%			[NewNeuron1,NewNeuron2]
	%		end,
	%		Mutation = #{type => neuro_link, link_from => NeuronIn#neuron_classic_phenotype.id, link_to => NeuronOut#neuron_classic_phenotype.id},
	%		{Genotype#genotype{neurons=lists:keysort(3,MutatedNeurons++RemainNeurons)}, Mutations ++ [Mutation]};
	%	true->{Genotype, Mutations}
	%end.
	ok.

add_sensor_link({Genotype, Mutations}, Constraint)->
	%{plast,Plasts}=lists:keyfind(plast,1,Constraint),
	%#genotype{cortex=Cortex,sensors=Sensors,neurons=Neurons}=Genotype,
	%SensorId,NeuronId}={?RANDCHOOSE(Cortex#cortex_phenotype.sensorsIds),?RANDCHOOSE(Cortex#cortex_phenotype.neuronsIds)},
	%PredSensor=fun(Sensor)->Sensor#sensor_phenotype.id==SensorId end,
	%{[Sensor],OtherSensors}=lists:partition(PredSensor,Sensors),
	%PredNeuron=fun(Neuron)->Neuron#neuron_classic_phenotype.id==NeuronId end,
	%{[Neuron],OtherNeurons}=lists:partition(PredNeuron,Neurons),
	%case exist_link([Sensor,Neuron]) of
	%	false->
	%		{NewSensor,NewNeuron}=connect(Sensor,Neuron,Plasts),
	%		Mutation = #{type => sensor_link, link_from => Sensor#sensor_phenotype.id, link_to => Neuron#neuron_classic_phenotype.id},
	%		{Genotype#genotype{sensors=OtherSensors++[NewSensor],neurons=lists:keysort(3,[NewNeuron]++OtherNeurons)}, Mutations ++ [Mutation]};
	%	true->{Genotype, Mutations}
	%end.
	ok.

add_neuron({Genotype, Mutations}, Constraint)->
	%{plast,Plasts}=lists:keyfind(plast,1,Constraint),
	%{af,Afs}=lists:keyfind(af,1,Constraint),
	%#genotype{sensors=Sensors,neurons=Neurons,cortex=Cortex}=Genotype,
	%Layers=genotype:get_layers(Genotype),
	%[H|T]=Layers,
	%case T of
	%	[]->Genotype;%se ho un solo layer allora niente
	%	_->L=?RANDCHOOSE(lists:droplast(Layers)),%ho almeno due strati
	%		NewNeuron=genotype:create_neuron(?RANDCHOOSE(Afs),L),
	%		case L of
	%			H->%ho scelto il primo strato
	%				Sensor=?RANDCHOOSE(Sensors),
	%				Neuron=?RANDCHOOSE([N||N<-Neurons,N#neuron_classic_phenotype.layer>L]),%seleziono un neurone da uno strato superiore
	%				RemainSensors=Sensors--[Sensor],
	%				RemainNeurons=Neurons--[Neuron],
	%				{NewSensor,NewNeuronConnIn}=connect(Sensor,NewNeuron,Plasts),
	%				{NewConnected,NewOut}=connect(NewNeuronConnIn,Neuron,Plasts),
	%				NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_phenotype.id]},
	%				Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_phenotype.id, connected_with => {Sensor#sensor_phenotype.id, Neuron#neuron_classic_phenotype.id}},
	%				{Genotype#genotype{sensors=RemainSensors++[NewSensor],neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewOut]),cortex=NewCortex}, Mutations ++ [Mutation]};
	%			_->
	%				NeuronSup=?RANDCHOOSE([N||N<-Neurons,N#neuron_classic_phenotype.layer>L]),%seleziono un neurone da uno strato superiore
	%				NeuronInf=?RANDCHOOSE([N||N<-Neurons,N#neuron_classic_phenotype.layer<L]),%seleziono un neurone da uno strato inferiore
	%				RemainNeurons=Neurons--[NeuronSup,NeuronInf],
	%				{NewInf,NewNeuronConnIn}=connect(NeuronInf,NewNeuron,Plasts),
	%				{NewConnected,NewSup}=connect(NewNeuronConnIn,NeuronSup,Plasts),
	%				NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_phenotype.id]},
	%				Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_phenotype.id, connected_with => {NeuronInf#neuron_classic_phenotype.id, NeuronSup#neuron_classic_phenotype.id}},
	%				{Genotype#genotype{neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewInf,NewSup]),cortex=NewCortex}, Mutations ++ [Mutation]}
	%		end
	%end.
	ok.

add_layer_neuron({Genotype, Mutations}, Constraint)->%aggiunge un nuovo strato tra due strati con un nuovo neurone connesso
	%{plast,Plasts}=lists:keyfind(plast,1,Constraint),
	%{af,Afs}=lists:keyfind(af,1,Constraint),
	%#genotype{sensors=Sensors,neurons=Neurons,cortex=Cortex}=Genotype,
	%Layers=genotype:get_layers(Genotype),
	%[H|_]=Layers,
	%case Layers of
	%	[1]->%se ho un solo strato,allora connetto tra sensori e neuroni
	%		Sensor=?RANDCHOOSE(Sensors),
	%		NewNeuron=genotype:create_neuron(?RANDCHOOSE(Afs),0.5),%lo vado a mettere come primo strato
	%		Neuron=?RANDCHOOSE(Neurons),%seleziono un neurone dal primo strato
	%		RemainSensors=Sensors--[Sensor],
	%		RemainNeurons=Neurons--[Neuron],%%cavo i neuroni selezionati
	%		{NewSensor,NewNeuronConnIn}=connect(Sensor,NewNeuron,Plasts),
	%		{NewConnected,NewOut}=connect(NewNeuronConnIn,Neuron,Plasts),
	%		NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_phenotype.id]},
	%		Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_phenotype.id, connected_with => {Sensor#sensor_phenotype.id, Neuron#neuron_classic_phenotype.id}},
	%		{Genotype#genotype{sensors=RemainSensors++[NewSensor],neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewOut]),cortex=NewCortex}, Mutations ++ [Mutation]};
	%	Layers->%altrimenti
	%		L=?RANDCHOOSE(lists:droplast(Layers)),%seleziona uno strato interno tranne l'ultimo
	%		case ?RANDCHOOSE([0,1]) of%lancio una moneta
	%			0->%lo inserisco prima di L
	%				case L of
	%					H->%se L è il primo strato
	%						Sensor=?RANDCHOOSE(Sensors),
	%						NewLayer=(L+0)/2,
	%						NewNeuron=genotype:create_neuron(?RANDCHOOSE(Afs),NewLayer),
	%						SupNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron_classic_phenotype.layer==L]),%seleziona un neurone casualmente allo strato successivo
	%						RemainSensors=Sensors--[Sensor],
	%						RemainNeurons=Neurons--[SupNeuron],
	%						{NewSensor,NewNeuronConnIn}=connect(Sensor,NewNeuron,Plasts),
	%						{NewConnected,NewOut}=connect(NewNeuronConnIn,SupNeuron,Plasts),
	%						NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_phenotype.id]},
	%						Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_genotype.id, connected_with => {Sensor#sensor_phenotype.id, SupNeuron#neuron_classic_genotype.id}},
	%						{Genotype#genotype{sensors=RemainSensors++[NewSensor],neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewOut]),cortex=NewCortex}, Mutations ++ [Mutation]};
	%					_->
	%					{InfLayers,_}=lists:splitwith(fun(N)->N<L end,Layers--[L]),%prendi gli strati inferiori
	%					LastLayer=lists:last(InfLayers),
	%					NewLayer=(L+LastLayer)/2,%lo sto inserendo prima di L
	%					NewNeuron=genotype:create_neuron(?RANDCHOOSE(Afs),NewLayer),%lo vado a mettere in mezzo tra L e L+1
	%					InfNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron_classic_genotype.layer==LastLayer]),%seleziona un neurone casualmente tra i neurone dello strato PRIMA di L
	%					SupNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron_classic_genotype.layer==L]),%seleziona un neurone casualmente tra i neurone dell strato L
	%					RemainNeurons=Neurons--[InfNeuron,SupNeuron],%%cavo i neuroni selezionati
	%					{NewInf,NewNeuronConnIn}=connect(InfNeuron,NewNeuron,Plasts),
	%					{NewConnected,NewSup}=connect(NewNeuronConnIn,SupNeuron,Plasts),
	%					NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_genotype.id]},
	%					Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_genotype.id, connected_with => {InfNeuron#neuron_classic_genotype.id, SupNeuron#neuron_classic_genotype.id}},
	%					{Genotype#genotype{neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewInf,NewSup]),cortex=NewCortex}, Mutations ++ [Mutation]}
	%				end;
	%			1->%lo inserisco dopo di L
	%				{_,SupLayers}=lists:splitwith(fun(N)->N<L end,Layers--[L]),%prendi gli strati superiori
	%				NextLayer=hd(SupLayers),
	%				NewLayer=(L+NextLayer)/2,%lo sto inserendo dopo di L
	%				NewNeuron=genotype:create_neuron(?RANDCHOOSE(Afs),NewLayer),%lo vado a mettere in mezzo tra L e L+1
	%				SupNeuron=?RANDCHOOSE([Neuron||Neuron<-Neurons,Neuron#neuron_classic_genotype.layer==NextLayer]),%seleziona un neurone casualmente tra i neurone dello strato SUCCESSIVO ad L
	%				RemainNeurons=Neurons--[InfNeuron,SupNeuron],%%cavo i neuroni selezionati
	%				{NewInf,NewNeuronConnIn}=connect(InfNeuron,NewNeuron,Plasts),
	%				{NewConnected,NewSup}=connect(NewNeuronConnIn,SupNeuron,Plasts),
	%				NewCortex=Cortex#cortex_phenotype{neuronsIds=Cortex#cortex_phenotype.neuronsIds++[NewNeuron#neuron_classic_genotype.id]},
	%				Mutation = #{type => add_neuron, neuron_id => NewNeuron#neuron_classic_genotype.id, connected_with => {InfNeuron#neuron_classic_genotype.id, SupNeuron#neuron_classic_genotype.id}},
	%				{Genotype#genotype{neurons=lists:keysort(3,RemainNeurons++[NewConnected,NewInf,NewSup]),cortex=NewCortex}, Mutations ++ [Mutation]}
	%		end
	%end.
	ok.	

%connect(N1,N2,Plasts)when N1#neuron_classic_genotype.layer<N2#neuron_classic_genotype.layer->%è una connessione in avanti
	%Plast=plasticity:random_plasticity(Plasts),
	%NewN1=N1#neuron_classic_phenotype{fanouts=N1#neuron_classic_phenotype.fanouts++[N2#neuron_classic_genotype.id]},
	%NewN2=N2#neuron{faninsWeights=N2#neuron.faninsWeights++[genotype:create_weight(Plast,N1)]},
	%NewN2 = null,
	%{NewN1,NewN2};
%connect(N1,N2,Plasts)when N1#neuron_classic_genotype.layer>=N2#neuron_classic_genotype.layer->%è una connessione all'indietro
	%Plast=plasticity:random_plasticity(Plasts),
	%NewN1=N1#neuron_classic_phenotype{roouts=N1#neuron_classic_phenotype.roouts++[N2#neuron_classic_genotype.id]},
	%NewN2=none, %N2#neuron{roinsWeights=N2#neuron.roinsWeights++[genotype:create_weight(Plast,N1)]},
	%{NewN1,NewN2};
%connect(Sensor,Neuron,Plasts) when is_record(Sensor,sensor_genotype)->
	%Plast=plasticity:random_plasticity(Plasts),
	%NewSensor=Sensor#sensor_phenotype{fanouts=Sensor#sensor_phenotype.fanouts++[Neuron#neuron_classic_genotype.id]},
	%NewNeuron=none, %Neuron#neuron{faninsWeights=Neuron#neuron.faninsWeights++[genotype:create_weight(Plast,Sensor)]},
	%{NewSensor,NewNeuron}.


%exist_link([#neuron_classic_phenotype{id=Id,roouts=RoOuts}])->
%	lists:member(Id,RoOuts);
%exist_link([#sensor_phenotype{fanouts=Outs},#neuron_classic_phenotype{id=Id}])->
%	lists:member(Id,Outs);
%exist_link([#neuron_classic_phenotype{layer=L1,fanouts=Outs},#neuron_classic_phenotype{id=Id2,layer=L2}])when L1<L2->
%	lists:member(Id2,Outs);
%exist_link([#neuron_classic_phenotype{id=Id1,layer=L1},#neuron_classic_phenotype{layer=L2,roouts=RoOuts}])when L1>=L2->
%	lists:member(Id1,RoOuts).