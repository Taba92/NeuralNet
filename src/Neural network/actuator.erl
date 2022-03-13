-module(actuator).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{scapeId,received,phenotype}).
-include("utils.hrl").
-include("phenotype.hrl").

init(Phenotype) when is_record(Phenotype, actuator_phenotype)->
	#actuator_phenotype{id = Id} = Phenotype,
	gen_server:start_link({local,Id}, ?MODULE, [Phenotype], []);
init([Phenotype])->
	State = #state{received = [], phenotype = Phenotype},
	{ok,State}.

terminate(normal, _) -> ok.

%Public api, to interact with this node
handle_call(get, _, State)->
	#state{phenotype = Phenotype} = State,
	{reply, Phenotype, State};
handle_call({update, {set_scape, Scape}}, _, State)->
	{reply, ok, State#state{scapeId = Scape}};
handle_call({add_synapses, {IdFrom, IdTo, Tag, _Weight, _Modulation, _ConnectionDirection}}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#actuator_phenotype{id = Id, input_elements_data = InputSynapses, output_elements_ids = OutputSynapse} = Phenotype,
	%Check if the node will be the sender or the receiver
	NewPhenotype = case Id of
						%Sender
						IdFrom -> 
							Phenotype#actuator_phenotype{output_elements_ids = OutputSynapse ++ [IdTo]};
						% Receiver
						IdTo ->
							{NodeTypeFrom, actuator} = Tag,
							Phenotype#actuator_phenotype{input_elements_data = InputSynapses ++ [{IdFrom, NodeTypeFrom}]}
					end,
	{reply, ok, State#state{phenotype = NewPhenotype}};
handle_call({delete_synapses, IdFrom, IdTo}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#actuator_phenotype{id = Id, input_elements_data = InputSynapses, output_elements_ids = OutputSynapse} = Phenotype,
	NewPhenotype = case Id of
						%Sender
						IdFrom ->
							Phenotype#actuator_phenotype{output_elements_ids = OutputSynapse -- [IdTo]};
						%Receiver
						IdTo ->
							%Get the synapse of sender node
							Synapse = lists:keyfind(IdFrom, 1, InputSynapses),
							Phenotype#actuator_phenotype{input_elements_data = InputSynapses -- [Synapse]}
					end,
	{reply, ok, State#state{phenotype = NewPhenotype}}.
%%%

handle_cast({neuron, Term, NId, forward_fit, Signal}, State) ->
	#state{scapeId = Scape, received = Recv, phenotype = Phenotype}=State,
	#actuator_phenotype{id = Id, fit_directives = Funs, input_elements_data = InputSynapse, output_elements_ids = OutputSynapse} = Phenotype,
	NewRecv = Recv ++ [{NId, Term, Signal}],
	NewState = case length(NewRecv) == length(InputSynapse) of
				true->
					OrderedSignal = nn_service:order_by_keylist(InputSynapse, NewRecv),
					%1) Function pipes are function that take a vector of numbers
					ProcessedSignal = nn_service:apply_directives_pipe(OrderedSignal, Funs),
					%io:fwrite("SIGNAL: ~p~n",[utils:get_BMU(ProcessedSignal)]),
					%2) Interract with the scape for evaluate the output
					{Flag, Msg} = gen_server:call(Scape, {action_fit, ProcessedSignal}, infinity),
					%3) Send to receivers the msg
					[gen_server:cast(OutputId, {fit, Id, Flag, Msg}) || OutputId <- OutputSynapse],
					State#state{received = []};
				false->
					State#state{received = NewRecv}
			end,
	{noreply, NewState};
handle_cast({neuron, Term, NId, forward_fit_predict, Signal}, State) ->
	#state{scapeId = Scape, received=Recv, phenotype = Phenotype} = State,
	#actuator_phenotype{id = Id, real_directives = Funs, input_elements_data = InputSynapse, output_elements_ids = OutputSynapse} = Phenotype,
	NewRecv = Recv ++ [{NId, Term, Signal}],
	NewState = case length(NewRecv) == length(InputSynapse) of
				true->
					OrderedSignal = nn_service:order_by_keylist(InputSynapse, NewRecv),
					%1) Function pipes are function that take a vector of numbers
					ProcessedSignal = nn_service:apply_directives_pipe(OrderedSignal, Funs),
					%2) Interract with the scape for evaluate the output
					{Flag, Msg} = gen_server:call(Scape, {action_fit_predict, ProcessedSignal}, infinity),
					%3) Send to receivers the msg
					[gen_server:cast(OutputId, {fit_predict, Id, Flag, Msg}) || OutputId <- OutputSynapse],
					State#state{received = []};
				false->
					State#state{received = NewRecv}
			end,
	{noreply, NewState};
handle_cast({neuron, Term, NId, forward_predict, Signal}, State) ->
	#state{scapeId = Scape, received = Recv, phenotype = Phenotype} = State,
	#actuator_phenotype{id = Id, real_directives = Funs, input_elements_data = InputSynapse, output_elements_ids = OutputSynapse} = Phenotype,
	NewRecv = Recv ++ [{NId, Term, Signal}],
	NewState = case length(NewRecv) == length(InputSynapse) of
				true->
					OrderedSignal = nn_service:order_by_keylist(InputSynapse, NewRecv),
					% Function pipes are function that take a vector of numbers
					ProcessedPred = nn_service:apply_directives_pipe(OrderedSignal, Funs),
					%2) Send to receivers the msg
					[gen_server:cast(OutputId, {predict, Id, ProcessedPred}) || OutputId <- OutputSynapse],
					State#state{received = []};
				false->
					State#state{received = NewRecv}
			end,
	{noreply, NewState}.

