-module(cortex).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2]).
-record(state, {received, phenotype}).
-include("utils.hrl").
-include("phenotype.hrl").

init(Phenotype) when is_record(Phenotype, cortex_phenotype) ->
	#cortex_phenotype{id = Id} = Phenotype,
	gen_server:start_link({local, Id}, ?MODULE, [Phenotype], []);
init([Phenotype]) when is_record(Phenotype, cortex_phenotype) ->
	State = #state{received = [], phenotype = Phenotype},
	{ok, State}.

terminate(normal, _) -> ok.

%% Public api for interract with this node, typical used throught the phenotype module
handle_call(get, _ , State) ->
	#state{phenotype = Phenotype} = State,
	{reply, Phenotype, State};
handle_call({update, {set_agent_id, AgentId}}, _, State) ->
	#state{phenotype = Phenotype} = State,
	NewPhenotype = Phenotype#cortex_phenotype{agent_id = AgentId},
	{reply, ok, State#state{phenotype = NewPhenotype}};
handle_call({add_synapses, {IdFrom, IdTo, Tag, Weight, Modulation, ConnectionDirection}}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#cortex_phenotype{id = Id, input_elements_data = InputSynapses, output_elements_ids = OutputSynapse} = Phenotype,
	%Check if the node will be the sender or the receiver
	NewPhenotype = case Id of
						%Sender
						IdFrom -> 
							Phenotype#cortex_phenotype{output_elements_ids = OutputSynapse ++ [IdTo]};
						% Receiver
						IdTo ->
							{NodeTypeFrom, cortex} = Tag,
							Phenotype#cortex_phenotype{input_elements_data = InputSynapses ++ [{IdFrom, NodeTypeFrom}]}
					end,
	{reply, ok, State#state{phenotype = NewPhenotype}};
handle_call({delete_synapses, IdFrom, IdTo}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#cortex_phenotype{id = Id, input_elements_data = InputSynapses, output_elements_ids = OutputSynapse} = Phenotype,
	NewPhenotype = case Id of
						%Sender
						IdFrom ->
							Phenotype#cortex_phenotype{output_elements_ids = OutputSynapse -- [IdTo]};
						%Receiver
						IdTo ->
							%Get the synapse of sender node
							Synapse = lists:keyfind(IdFrom, 1, InputSynapses),
							Phenotype#cortex_phenotype{input_elements_data = InputSynapses -- [Synapse]}
					end,
	{reply, ok, State#state{phenotype = NewPhenotype}}.
%%%

%% Internal network api, used during evaluation cycles
handle_cast(fit_cycle, State) ->
	#state{phenotype = Phenotype} = State,
	#cortex_phenotype{output_elements_ids = SensorsIds} = Phenotype,
	[gen_server:cast(Sensor, sync_fit) || Sensor <- SensorsIds],
	{noreply, State};
handle_cast({predict_cycle, Signal}, State) ->
	#state{phenotype = Phenotype} = State,
	#cortex_phenotype{output_elements_ids = SensorsIds} = Phenotype,
	[gen_server:cast(Sensor, {sync_predict, Signal}) || Sensor <- SensorsIds],
	{noreply, State};
handle_cast(fit_predict_cycle, State) ->
	#state{phenotype = Phenotype} = State,
	#cortex_phenotype{output_elements_ids = SensorsIds} = Phenotype,
	[gen_server:cast(Sensor, sync_fit_predict) || Sensor <- SensorsIds],
	{noreply, State};
handle_cast({fit, Id, Flag, Msg}, State) ->
	#state{received = Recv, phenotype = Phenotype} =State,
	#cortex_phenotype{agent_id = AgentId, fit_directives = Funs, input_elements_data = ActuatorsData} = Phenotype,
	NewRecv = Recv ++ [{Id, null, Msg}],
	NewState = case length(NewRecv) == length(ActuatorsData) of
					true ->
						OrderedMsgs = nn_service:order_by_keylist(ActuatorsData, NewRecv),
						Msgs = [Message || {_, _, Message} <- OrderedMsgs],
						%%Functions pipes are functions that takes a vector of msgs(list of maps, see scapes return)
						ProcessedMsgs = nn_service:apply_directives_pipe(Msgs, Funs),
						%Send to the agent the result
						AgentId ! {fit, Flag, ProcessedMsgs},
						State#state{received = []};
					false ->
						State#state{received = NewRecv}
			end,
	{noreply, NewState};
handle_cast({fit_predict, Id, Flag, Msg}, State) ->
	#state{received = Recv, phenotype = Phenotype} = State,
	#cortex_phenotype{agent_id = AgentId, real_directives = Funs, output_elements_ids = ActuatorsData} = Phenotype,
	NewRecv = Recv ++ [{Id, null, Msg}],
	NewState = case length(NewRecv) == length(ActuatorsData) of
					true ->
						OrderedMsgs = nn_service:order_by_keylist(ActuatorsData, NewRecv),
						Msgs = [Message || {_, _, Message} <- OrderedMsgs],
						%%Functions pipes are functions that takes a vector of msgs(list of maps, see scapes return)
						ProcessedMsgs = nn_service:apply_directives_pipe(Msgs, Funs),
						%Send to the agent the result
						AgentId ! {fit_predict, Flag, ProcessedMsgs},
						State#state{received = []};
					false -> 
						State#state{received = NewRecv}
			end,
	{noreply, NewState};
handle_cast({predict, Id, Pred}, State) ->
	#state{received = Recv, phenotype = Phenotype} = State,
	#cortex_phenotype{agent_id = AgentId, real_directives = Funs, output_elements_ids = ActuatorsData} = Phenotype,
	NewRecv = Recv ++ [{Id, null, Pred}],
	NewState=case length(NewRecv) == length(ActuatorsData) of
				true->
					OrderedMsgs = nn_service:order_by_keylist(ActuatorsData, NewRecv),
					Msgs = [Message || {_, _, Message} <- OrderedMsgs],
					%%Functions pipes are functions that takes a vector of msgs(list of maps, see scapes return)
					ProcessedMsgs = nn_service:apply_directives_pipe(Msgs, Funs),
					io:fwrite("PREDICT: ~p~n",[ProcessedMsgs]),
					AgentId ! {prediction, ProcessedMsgs},
					State#state{received = []};
				false ->
					State#state{received = NewRecv}
			end,
	{noreply, NewState}.
%%
