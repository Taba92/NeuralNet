-module(neuron).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{received, recurrent_received, oldBias, old_input_synapses, old_recurrent_input_synapses, last_output, lasts_signals, phenotype}).
-include("utils.hrl").
-include("phenotype.hrl").

init(Phenotype) when is_record(Phenotype, neuron_classic_phenotype) ->
	#neuron_classic_phenotype{id = Id} = Phenotype,
	gen_server:start_link({local,Id}, ?MODULE, [Phenotype], []);
init([Phenotype]) ->
	#neuron_classic_phenotype{recurrent_input_signals_data = RecurrentInputSynapses} = Phenotype,
	N = length(RecurrentInputSynapses),
	State = #state{received = [], recurrent_received = lists:duplicate(N, nil), phenotype = Phenotype},
	{ok, State}.

handle_call(get, _, State) ->
	#state{phenotype = Phenotype} = State,
	{reply, Phenotype, State};
handle_call({update, backup_weights}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#neuron_classic_phenotype{bias = Bias, input_elements_data = InputSynapses, recurrent_input_signals_data = RecurrentInputSynapses} = Phenotype,
	{reply, ok, State#state{oldBias = Bias, old_input_synapses = InputSynapses, old_recurrent_input_synapses = RecurrentInputSynapses}};
handle_call({update, restore_weights}, _, State) ->
	#state{oldBias = OldBias, old_input_synapses = OldInputSynapses, old_recurrent_input_synapses = OldRecurrentInputSynapses, phenotype = Phenotype} = State,
	NewPhenotype = Phenotype#neuron_classic_phenotype{bias = OldBias, input_elements_data = OldInputSynapses, recurrent_input_signals_data = OldRecurrentInputSynapses},
	{reply, ok ,State#state{phenotype = NewPhenotype}};
handle_call({update, {perturb_weights, NumberOfElements, StepnessOfPerturbation}}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#neuron_classic_phenotype{bias = Bias, input_elements_data = InputSynapses, recurrent_input_signals_data = RecurrentInputSynapses} = Phenotype,
	NewState = case ?PROB(NumberOfElements) of
				true ->
					PerturbationProbability = (length(InputSynapse) + length(RecurrentInputSynapses) + 1) * StepnessOfPerturbation /100,
					NewBias = perturbate_bias(Bias, PerturbationProbability),
					NewInputSynapses = [perturbate_weight(InputSynapse, PerturbationProbability) || InputSynapse <- InputSynapses],
					NewRecurrentInputSynapses = [perturbate_weight(RecurrentInputSynapse, PerturbationProbability) || RecurrentInputSynapse <- RecurrentInputSynapses],
					NewPhenotype = Phenotype#neuron_classic_phenotype{bias = NewBias, input_elements_data = NewInputSynapses, recurrent_input_signals_data = NewRecurrentInputSynapses},
					State#state{phenotype = NewPhenotype};
				false -> 
					State
			end,
	{reply, ok, NewState};
handle_call({add_synapses, {IdFrom, IdTo, Tag, Weight, Modulation, ConnectionDirection}}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#neuron_classic_phenotype{id = Id, input_elements_data = InputSynapses, output_elements_ids = OutputSynapses, recurrent_input_signals_data = RecurrentInputSynapses, recurrent_output_elements_ids = RecurrentOutputSynapses} = Phenotype,
	%Check if the node will be the sender or the receiver and if is a forward or recurrent synapse
	NewPhenotype = case {Id, ConnectionDirection} of
						%Sender and forward synapse
						{IdFrom, forward} -> 
							Phenotype#neuron_classic_phenotype{output_elements_ids = OutputSynapse ++ [IdTo]};
						%Sender and recurrent synapse
						{IdFrom, recurrent} ->
							Phenotype#neuron_classic_phenotype{recurrent_output_elements_ids = RecurrentOutputSynapses ++ [IdTo]};
						% Receiver and forward synapse
						{IdTo, forward} ->
							{NodeTypeFrom, neuron} = Tag,
							Phenotype#neuron_classic_phenotype{input_elements_data = InputSynapses ++ [{IdFrom, NodeTypeFrom, Weight, Modulation}]};
						% Receiver and recurrent synapse
						{IdTo, recurrent} ->
							{NodeTypeFrom, neuron} = Tag,
							Phenotype#neuron_classic_phenotype{recurrent_input_elements_data = RecurrentInputSynapses ++ [{IdFrom, NodeTypeFrom, Weight, Modulation}]};
					end,
	{reply, ok, State#state{phenotype = NewPhenotype}};
handle_call({delete_synapses, IdFrom, IdTo}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#neuron_classic_phenotype{id = Id, input_elements_data = InputSynapses, output_elements_ids = OutputSynapses, recurrent_input_signals_data = RecurrentInputSynapses, recurrent_output_elements_ids = RecurrentOutputSynapses} = Phenotype,
	%Check if the node will be the sender or the receiver and if is a forward or recurrent synapse
	NewPhenotype = case Id of
						%Sender 
						IdFrom -> 
							%Check if is an forward or recurrent output synapse
							case lists:member(IdTo, OutputSynapse) of
								true ->
									Phenotype#neuron_classic_phenotype{output_elements_ids = OutputSynapse -- [IdTo]};
								false ->
									Phenotype#neuron_classic_phenotype{recurrent_output_elements_ids = RecurrentOutputSynapse -- [IdTo]}
							end;
						% Receiver 
						IdTo ->
							%Check if is an forward or recurrent input synapse
							case lists:keymember(IdFrom, 1 , InputSynapses) of
								true -> 
									Synapse = lists:keyfind(IdFrom, 1, InputSynapses),
									Phenotype#neuron_classic_phenotype{input_elements_data = InputSynapses -- [Synapse]};
								false ->
									RecurrentSynapse = lists:keyfind(IdFrom, 1, RecurrentInputSynapses),
									Phenotype#neuron_classic_phenotype{recurrent_input_elements_data = RecurrentInputSynapses -- [RecurrentSynapse]};
							end
					end,
	{reply, ok, State#state{phenotype = NewPhenotype}};

terminate(normal, _) -> ok.

handle_cast({ElType, FromLayer, IdFrom, FwdType, Signal}, State) when ElType == sensor;ElType == neuron ->
	%io:fwrite("SIGNAL IN ~p~n",[{ElType,IdFrom,FromLayer,Signal}]),
	#state{received = Recv,recurrent_received = RecurrentRecv, phenotype = Phenotype} = State,
	#neuron_classic_phenotype{id = Id, layer = Layer, activation_function = ActivationFunction, bias = Bias, 
							input_elements_data = InputSynapses, output_elements_ids = OutputSynapse, 
							recurrent_input_elements_data = RecurrentInputSynapses, recurrent_output_elements_ids = RecurrentOutputSynapses} = Phenotype,
	{NewRecv, NewRecurrentRecv} = case FromLayer >= Layer of
								true -> 
									{Recv, RoRecv ++ [{IdFrom, Signal}]};
								false ->
									{Recv ++ [{IdFrom, Signal}], RoRecv}
							end,
	%{PrunRecv,PrunRoRecv}={NewRecv,NewRoRecv},
	%Could be doubled input signals!
	{PrunRecv, PrunRecurrentRecv} = {prunSignals(NewRecv, length(InputSynapses)), prunSignals(NewRecurrentRecv, length(RecurrentInputSynapses))},
	NewState = case length(PrunRecv) == length(InputSynapses) andalso length(PrunRecurrentRecv) == length(RecurrentInputSynapses) of
		        	true ->
		        		Dot = aggregate(PrunRecv, InputSynapses) + aggregate(PrunRecurrentRecv, RecurrentInputSynapses),
						OutPut = ?ACTIVATION_FUNCTION_MODULE:ActivationFunction(Dot + Bias),
						NewPhenotype = learn(Phenotype, [OutPut], PrunRecv, PrunRecurrentRecv),
						[gen_server:cast(OutputId, {neuron, Layer, Id, FwdType, [OutPut]}) || OutputId <- OutputSynapse ++ RecurrentOutputSynapses],
						State#state{last_output = {Dot, OutPut}, last_signals = {PrunRecv, PrunRecurrentRecv}, received = [], recurrent_received = [], phenotype = NewPhenotype};
					false ->
						State#state{received = NewRecv, recurrent_received = NewRecurrentRecv}
			end,
	{noreply, NewState}.

%%private methods

%If there is a number of input signals greater then the number of input synapses, drop the exceed input signals
prunSignals(Signals, NumberOfInputSynapses) ->
	case length(Signals) =< NumberOfInputSynapses of
		true -> Signals;
		false -> {LenSignals, _} = lists:split(NumberOfInputSynapses, Signals),
				  LenSignals
	end.

%In case of recurrent net and start fit cycle 
aggregate([nil | _], _) -> 0;
%Normal case
aggregate(Signals, Synapses) ->
	aggregate(Signals, Synapses, 0). 
aggregate([], _, Acc) -> Acc;
aggregate([{Id, Signal}| T], Synapses, Acc)->
	{Id, _, Weigth, _} = lists:keyfind(Id, 1, Synapses),
	aggregate(T, Synapses, Acc + dot(Signal, Weight, Acc)).
	

perturbate_weight({Id, NodeTypeFrom, Weight, Modulation}, PerturbationProbability)->
	case ?PROB(PerturbationProbability) of
		true -> 
			{Id, NodeTypeFrom, perturbate_vals(Weight), Modulation};
		false -> 
			{Id, NodeTypeFrom, Weight, Modulation}
	end.

perturbate_vals([]) -> [];
perturbate_vals([H | T]) -> [?NN_SERVICE_MODULE:perturbate(H) | perturbate_vals(T)].

perturbate_bias(Bias, PerturbationProbability)->
	case ?PROB(PerturbationProbability) of
		true -> 
			?NN_SERVICE_MODULE:perturbate(Bias);
		false -> 
			Bias
	end.

dot(Signal, Weight, _) when length(Sig) /= length(Weight) ->
	error(bad_signal_for_weight);
dot([], [], Dot) -> 
	Dot;
%Try to perform the dot operator.
%In case of failure, it return randomly a output limit
dot([SignalEl |TailSignal], [WeightEl | TailWeight], Acc) ->
	try dot(TailSignal, TailWeight, Acc + S * W ) of Val -> Val catch _:_ -> ?RANDCHOOSE([-?SAT_LIMIT, ?SAT_LIMIT]) end.

%Modulate a weight with modulation data
learn(Phenotype, Output, Recv, [nil | _]) -> %è all'inizio,non ho segnali ricorsivi in entrata nel neurone!
	#neuron_classic_phenotype{input_elements_data = InputSynapses} = Phenotype,
	%io:fwrite("*****~nGENO: ~p~n INS: ~p~n RECV: ~p~n*****~n",[GenoType,Ins,Recv]),
	NewInputSynapses = plasticity:apply_plasticity(InputSynapses, Recv, Output),
	Phenotype#neuron_classic_phenotype{input_elements_data = NewInputSynapses};
learn(Phenotype, Output, Recv, RecurrentRecv) ->%%inizia ad avere segnali ricorsivi in entrata
	#neuron_classic_phenotype{input_elements_data = InputSynapses, recurrent_input_elements_data = RecurrentInputSynapses} = Phenotype,
	%io:fwrite("****~nGENO: ~p~n INS: ~p~n RECV: ~p~n ROINS: ~p~n RORECV: ~p~n****~n",[GenoType,Ins,Recv,RoIns,RoRecv]),
	NewInputSynapses = plasticity:apply_plasticity(InputSynapses, Recv, Output),
	NewRecurrentInputSynapses = plasticity:apply_plasticity(RecurrentInputSynapses, RecurrentRecv, Output),
	Phenotype#neuron_classic_phenotype{input_elements_data = NewInputSynapses, recurrent_input_elements_data = NewRecurrentInputSynapses}.
