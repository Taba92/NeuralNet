-module(regression).
-export([extract_info/1, init/1, handle_call/3]).
-define(METRICS, regression_metrics).
-include("Scape/scape.hrl").

init([Dataset, HasHeader, Labelled, Cursor, DatasetActions]) -> 
	StartState = #state{dataset = Dataset, has_header = HasHeader, cursor = Cursor, dataset_actions = DatasetActions, 
						num_line_readed = 0, fit = 0, loss = 0, labelled = Labelled},
	%Drop the header if the dataset have one
	InitialState = scape_service:drop_header(StartState),
	{ok, InitialState}.

handle_call(extract_info, _, State) ->
	MapInfo = extract_info(State),
	{reply, MapInfo, State#state{info = MapInfo}};

handle_call({set_limit, Limit}, _, State)->
	{reply, ok, State#state{limit = round(Limit)}};

handle_call(reset, _, State) ->
	%Reset the dataset
	ResettedState = scape_service:reset_scape(State),
	%Reset the state
	InitState = ResettedState#state{current = undefined, fit = 0, loss = 0},
	{reply, ok, InitState};

handle_call(sense, _, State) ->
	#state{dataset = Dataset, dataset_actions = DatasetActions, cursor = Cursor, labelled = Labelled} = State,
	#dataset_actions{read_action = ReadFun, parse_line_action = ParseLineAction} = DatasetActions,
	%%Read next record in the dataset
	{Record, NewCursor} = ReadFun(Dataset, Cursor),
	%Parse the record
	ParsedRecord = ParseLineAction(Record),
	%Extract features and target from the parsed record
	{Features, _ } = scape_service:extract_features_and_target(Labelled, ParsedRecord),
	NewState = State#state{current = Record, cursor = NewCursor},
	{reply, Features, NewState};

handle_call({action_fit, Predict}, _, State)->
	#state{current = Record, labelled = Labelled, num_line_readed = NumLineReaded, cursor = Cursor, limit = Limit, 
		   dataset_actions = DatasetActions, dataset = Dataset, fit = FitAcc, loss = LossAcc} = State,
	#dataset_actions{parse_line_action = ParseLineFun, is_finished_action = IsFinishedFun} = DatasetActions,
	%Parse the current readed record
	ParsedRecord = ParseLineFun(Record),
	%Extract target from the parsed record
	{_, Target} = scape_service:extract_features_and_target(Labelled, ParsedRecord),
	PartialLoss = ?METRICS:smape(Predict, Target),%%potrà usare differenti loss function!
	PartialFit = 1 - ?METRICS:smape(Predict, Target),
	case IsFinishedFun(Dataset, Cursor) of
		true ->
			Fitness = (FitAcc + PartialFit) / NumLineReaded,
			Loss = (LossAcc + PartialLoss) / NumLineReaded,
			%Reset the scape
			ResettedState = scape_service:reset_scape(State),
			NewState = ResettedState#state{fit = 0, loss = 0},
			Msg = #{type => regression, partial_fit => PartialFit, partial_loss => PartialLoss, loss => Loss,
					fitness => Fitness, target => Target, predict => Predict},
			{reply, {finish, Msg}, NewState};
		false ->
			case NumLineReaded == Limit of
				true->
					Fitness = (FitAcc + PartialFit) / NumLineReaded,
					Loss = (LossAcc + PartialLoss) / NumLineReaded,
					NewState = State#state{num_line_readed = 0, fit = 0, loss = 0},
					Msg = #{type => regression, partial_fit => PartialFit, partial_loss => PartialLoss, loss => Loss,
							fitness => Fitness, target => Target, predict => Predict},
					{reply, {finish, Msg}, NewState};
				false->
					NewState = State#state{num_line_readed = NumLineReaded + 1, fit = FitAcc + PartialFit, 
								loss = LossAcc + PartialLoss},
					Msg = #{type => regression, partial_fit => PartialFit, partial_loss => PartialLoss,
							target => Target, predict => Predict},
					{reply, {another, Msg}, NewState}
			end
	end;

handle_call({action_fit_predict, Predict}, _, State)->
	#state{current = Record, labelled = Labelled, dataset_actions = DatasetActions, cursor = Cursor, dataset = Dataset,num_line_readed = NumLineReaded} = State,
	#dataset_actions{parse_line_action = ParseLineFun, is_finished_action = IsFinishedFun} = DatasetActions,
	%Parse the current readed record
	ParsedRecord = ParseLineFun(Record),
	%Extract target from the parsed record
	{_, Target} = scape_service:extract_features_and_target(Labelled, ParsedRecord),
	Msg = #{type => regression, target => Target, predict => Predict},
	case IsFinishedFun(Dataset, Cursor) of
		true ->
			% Reset the dataset
			NewState = scape_service:reset_scape(State),
			{reply, {finish, Msg}, NewState};
		false ->
			NewState = State#state{num_line_readed = NumLineReaded + 1},
			{reply, {another, Msg}, NewState}
	end.

%%Assume that the State is the scape initial state!
extract_info(State) ->
	#state{dataset = Dataset, cursor = Cursor, labelled = Labelled, dataset_actions = DatasetActions, num_line_readed = NumLineReaded } = State,
	#dataset_actions{read_action = ReadFun, parse_line_action= ParseLineFun} = DatasetActions,
	% 1) Get the first line of the dataset
	{Line, NewCursor} = ReadFun(Dataset, Cursor),
	NewState = State#state{cursor = NewCursor, num_line_readed = NumLineReaded + 1},
	% 2) Get the number of features
	ParsedLine = ParseLineFun(Line),
	{Features, _} = scape_service:extract_features_and_target(Labelled, ParsedLine),
	NumFeatures = length(Features),
	% 3) Initialize the mins, maxs and sums vectors
	Mins = Maxs = Sums = Features,
	% 4) Initialize supports vectors for infos computation
	Scarti = lists:duplicate(NumFeatures, 0),
	% 5) Computes infos vectors
	% 5.1) Extract the first part of informations
	{NewMins, NewMaxs, NewSums, DatasetLen} = scape_service:extract_part_one(NewState, Mins, Maxs, Sums),
	% 5.2) Reset the dataset
	FirstResetState = scape_service:reset_scape(State),
	% 5.3) Extract the second part of informations
	Avgs = [Sum / DatasetLen || Sum <- NewSums],
	Stds = scape_service:extract_part_two(FirstResetState, Avgs, Scarti, DatasetLen),
	% 5.4) Reset the dataset
	scape_service:reset_scape(FirstResetState),
	#{mins => NewMins, maxs => NewMaxs, len => DatasetLen, num_features => NumFeatures, avgs => Avgs, stds => Stds}.