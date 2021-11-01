-module(classification).
-export([extract_info/1, init/1, handle_call/3]).
-define(METRICS, classification_metrics).
-include("Scape/scape.hrl").
-include("utils.hrl").

init([Dataset, HasHeader, Labelled, Cursor, DatasetActions]) -> 
	StartState = #state{dataset = Dataset, has_header = HasHeader, cursor = Cursor, dataset_actions = DatasetActions, 
						num_line_readed = 0, loss = 0, labelled = Labelled},
	%Drop the header if the dataset have one
	InitialState = scape_service:drop_header(StartState),
	{ok, InitialState}.

handle_call(extract_info, _, State) ->
	MapInfo = extract_info(State),
	Matrix = ?METRICS:create_matrix_confusion(erlang:map_get(classes, MapInfo)),
	{reply, MapInfo, State#state{info = MapInfo, matrix = Matrix }};

handle_call({set_limit, Limit}, _, State) ->
	{reply, ok, State#state{limit = round(Limit) }};

handle_call(reset, _, State) ->
	#state{info = MapInfo} = State,
	%Reset the dataset
	ResettedState = scape_service:reset_scape(State),
	%Recreate a new confusion matrix
	Matrix = ?METRICS:create_matrix_confusion(erlang:map_get(classes, MapInfo)),
	%Reset the state
	InitState = ResettedState#state{current = undefined, matrix = Matrix, loss = 0},
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

handle_call({action_fit, Prediction},_,State) ->
	#state{current = Record, labelled = Labelled, num_line_readed = NumLineReaded, cursor = Cursor, limit = Limit, info = MapInfo, 
		   dataset_actions = DatasetActions, dataset = Dataset, matrix = Matrix, loss = LossAcc} = State,
	#dataset_actions{parse_line_action = ParseLineFun, is_finished_action = IsFinishedFun} = DatasetActions,
	%Parse the current readed record
	ParsedRecord = ParseLineFun(Record),
	%Extract target from the parsed record
	{_, Target} = scape_service:extract_features_and_target(Labelled, ParsedRecord),
	#{encoding := Encoding, len := Len, classes := Classes} = MapInfo,
	Encode = ?DATA_PROCESSING_MODULE:encode(Target, Encoding),
	PartialLoss = ?METRICS:cross_entropy(Encode, Prediction),
	PartialFit = 1 - math_utils:manhattan_distance(Encode, Prediction),
	ClassChoose = ?DATA_PROCESSING_MODULE:decode(?DATA_PROCESSING_MODULE:most_likely(Prediction), Encoding),
	UpdateMatrix = ?METRICS:incr_cell_matrix(Target, ClassChoose, Matrix),
	case IsFinishedFun(Dataset, Cursor) of
		true ->
			Fitness = ?METRICS:f1_score_avg(Classes, UpdateMatrix),
			Loss = (PartialLoss + LossAcc) / Len,
			NewMatrix = ?METRICS:create_matrix_confusion(erlang:map_get(classes, MapInfo)),
			%Reset the dataset
			ResettedState = scape_service:reset_scape(State),
			NewState = ResettedState#state{matrix = NewMatrix, loss = 0},
			Msg = #{type => classification, partial_fit => PartialFit, partial_loss => PartialLoss,
					loss => Loss, fitness => Fitness, target => Encode, predict => Prediction},
			{reply, {finish, Msg}, NewState};
		false -> %% must evaluate if i reached the limit of readable records
			case NumLineReaded == Limit of
				true->%stop the dataset fit reading
					Fitness = ?METRICS:f1_score_avg(Classes, UpdateMatrix),
					Loss = (PartialLoss + LossAcc) / Len,
					NewMatrix = ?METRICS:create_matrix_confusion(erlang:map_get(classes, MapInfo)),
					NewState = State#state{num_line_readed = 0, matrix = NewMatrix, loss = 0},
					Msg = #{type => classification, partial_fit => PartialFit, partial_loss => PartialLoss, 
							loss => Loss, fitness => Fitness, target => Encode, predict => Prediction},
					{reply, {finish, Msg}, NewState};
				false->
					NewState = State#state{num_line_readed = NumLineReaded + 1, matrix = UpdateMatrix, 
								loss = LossAcc + PartialLoss},
					Msg = #{type => classification, partial_fit => PartialFit, partial_loss => PartialLoss,
							target => Encode, predict => Prediction},
					{reply, {another, Msg}, NewState}
			end
	end;

handle_call({action_fit_predict, Predict}, _, State) ->
	#state{current = Record, labelled = Labelled, dataset_actions = DatasetActions, cursor = Cursor, dataset = Dataset,num_line_readed = NumLineReaded} = State,
	#dataset_actions{parse_line_action = ParseLineFun, is_finished_action = IsFinishedFun} = DatasetActions,
	%Parse the current readed record
	ParsedRecord = ParseLineFun(Record),
	%Extract target from the parsed record
	{_, Target} = scape_service:extract_features_and_target(Labelled, ParsedRecord),
	Msg = #{type => classification, target => Target, predict => Predict},
	case IsFinishedFun(Dataset, Cursor) of
		true ->
			% Reset the dataset
			NewState = scape_service:reset_scape(State),
			{reply, {finish, Msg}, NewState};
		false ->
			NewState = State#state{num_line_readed = NumLineReaded + 1},
			{reply, {another, Msg}, NewState}
	end;

handle_call({action_predict, _}, _, State) ->
	{reply, ok, State}.

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
	SecondResetState = scape_service:reset_scape(FirstResetState),
	% 5.5) Extract information specifics for the classification
	Targets = extract_targets(SecondResetState),
	Encoding = data_processing:one_hot(Targets),
	NumClasses = length(Targets),
	% 5.6) Reset the dataset
	scape_service:reset_scape(FirstResetState),
	#{mins => NewMins, maxs => NewMaxs, len => DatasetLen, num_features => NumFeatures, num_classes => 
		NumClasses, classes => Targets, avgs => Avgs, stds => Stds, encoding => Encoding}.

extract_targets(State) ->
	extract_targets(State, []).
extract_targets(State, Targets) ->
	#state{dataset = Dataset, dataset_actions = DatasetActions, labelled = Labelled, cursor = Cursor, num_line_readed = NumLineReaded} = State,
	#dataset_actions{read_action = ReadFun, parse_line_action = ParseLineFun, 
					is_finished_action = IsFinishedFun} = DatasetActions,
	case IsFinishedFun(Dataset, Cursor) of
		true -> 
			Targets;
		false ->
			{Line, NewCursor} = ReadFun(Dataset, Cursor),
			ParsedLine = ParseLineFun(Line),
			{_, Target} = scape_service:extract_features_and_target(Labelled, ParsedLine),
			NewTargets = case lists:member(Target, Targets) of
							true -> Targets;
							false -> Targets ++ [Target]
						end,
			NewState = State#state{cursor = NewCursor, num_line_readed = NumLineReaded + 1},
			extract_targets(NewState, NewTargets)
	end.