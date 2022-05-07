-module(sensor).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{scapeId, phenotype}).
-include("utils.hrl").
-include("phenotype.hrl").

init(Phenotype) when is_record(Phenotype, sensor_phenotype)->
	#sensor_phenotype{id = Id} = Phenotype,
	gen_server:start_link({local,Id}, ?MODULE, [Phenotype], []);
init([Phenotype])->
	State = #state{phenotype = Phenotype},
	{ok, State}.

terminate(normal, _) -> ok.

handle_call(get, _, State) ->
	#state{phenotype = Phenotype} = State,
	{reply, Phenotype, State};
handle_call({update, {set_scape, Scape}}, _, State) ->
	{reply, ok, State#state{scapeId = Scape}};
handle_call({add_synapses, {IdFrom, IdTo, Tag, _Weight, _Modulation, _ConnectionDirection}}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#sensor_phenotype{id = Id, input_elements_data = InputSynapses, output_elements_ids = OutputSynapse} = Phenotype,
	%Check if the node will be the sender or the receiver
	NewPhenotype = case Id of
						%Sender
						IdFrom -> 
							Phenotype#sensor_phenotype{output_elements_ids = OutputSynapse ++ [IdTo]};
						% Receiver
						IdTo ->
							{NodeTypeFrom, sensor} = Tag,
							Phenotype#sensor_phenotype{input_elements_data = InputSynapses ++ [{IdFrom, NodeTypeFrom}]}
					end,
	{reply, ok, State#state{phenotype = NewPhenotype}};
handle_call({delete_synapses, IdFrom, IdTo, Tag}, _, State) ->
	#state{phenotype = Phenotype} = State,
	#sensor_phenotype{id = Id, input_elements_data = InputSynapses, output_elements_ids = OutputSynapse} = Phenotype,
	NewPhenotype = case Id of
						%Sender
						IdFrom ->
							Phenotype#sensor_phenotype{output_elements_ids = OutputSynapse -- [IdTo]};
						%Receiver
						IdTo ->
							%Get the synapse of sender node
							Synapse = lists:keyfind(IdFrom, 1, InputSynapses),
							Phenotype#sensor_phenotype{input_elements_data = InputSynapses -- [Synapse]}
					end,
	{reply, ok, State#state{phenotype = NewPhenotype}}.
%%%

handle_cast(sync_fit, State) ->
	#state{scapeId = Scape, phenotype = Phenotype} = State,
	#sensor_phenotype{id = Id, fit_directives = Funs, output_elements_ids = OutputSynapse} = Phenotype,
	Signal = gen_server:call(Scape, sense),
	%1) Function pipes are function that take a vector of numbers as first argument and other optionals args
	ProcessedSig = nn_service:apply_directives_pipe(Signal, Funs),
	[gen_server:cast(OutputId, {sensor, 0, Id, forward_fit, ProcessedSig}) || OutputId <- OutputSynapse],
	{noreply, State};
handle_cast(sync_fit_predict, State) ->
	#state{scapeId = Scape, phenotype = Phenotype} = State,
	#sensor_phenotype{id = Id, fit_directives = Funs, output_elements_ids = OutputSynapse} = Phenotype,
	Signal = gen_server:call(Scape, sense),
	%1) Function pipes are function that take a vector of numbers
	ProcessedSig = nn_service:apply_directives_pipe(Signal, Funs),
	[gen_server:cast(OutputId, {sensor, 0, Id, forward_fit_predict, ProcessedSig}) || OutputId <- OutputSynapse],
	{noreply, State};
handle_cast({sync_predict, Signal}, State) ->
	#state{phenotype = Phenotype} = State,
	#sensor_phenotype{id = Id, real_directives = Funs, output_elements_ids = OutputSynapse} = Phenotype,
	%1) Function pipes are function that take a vector of numbers
	ProcessedSig = nn_service:apply_directives_pipe(Signal, Funs),
	[gen_server:cast(OutputId, {sensor, 0, Id, forward_predict, ProcessedSig}) || OutputId <- OutputSynapse],
	{noreply, State}.
