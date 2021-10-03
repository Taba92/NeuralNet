-module(classification).
-export([extract_info/1, init/1, handle_call/3]).
-define(METRICS, classification_metrics).
-include("utils.hrl").
-include("Scape/scape.hrl").

init([Dataset, HasHeader, DatasetActions]) -> 
	#dataset_actions{read_action = ReadFun} = DatasetActions,
	%Drop the header if the dataset have one
	StartLineReaded = scape_utils:drop_header(HasHeader, Dataset, ReadFun),
	State = #state{dataset = Dataset, has_header = HasHeader, dataset_actions = DatasetActions, num_line_readed = StartLineReaded, loss = 0},
	{ok, State}.

handle_call(extract_info, _, State) ->
	MapInfo = extract_info(State),
	Matrix = ?METRICS:create_matrix_confusion(erlang:map_get(classes, MapInfo)),
	{reply, MapInfo, State#state{info = MapInfo, matrix = Matrix }};

handle_call({set_limit,Limit}, _, State) ->
	{reply, ok, State#state{limit = round(Limit) }};

handle_call(reset, _, State) ->
	#state{has_header = HasHeader, dataset_actions = DatasetActions, num_line_readed = NumLineReaded,
			dataset = Dataset, info = MapInfo} = State,
	#dataset_actions{read_action = ReadFun, reset_action = ResetFun} = DatasetActions,
	%Reset the dataset
	ResettedDataset = ResetFun(Dataset, NumLineReaded),
	%Recreate a new confusion matrix
	Matrix = ?METRICS:create_matrix_confusion(erlang:map_get(classes,MapInfo)),
	% Drop the header if the dataset have one
	StartLineReaded = scape_utils:drop_header(HasHeader, ResettedDataset, ReadFun),
	%Reset the state
	InitState = State#state{current = undefined, dataset = ResettedDataset, num_line_readed = StartLineReaded, matrix = Matrix, loss = 0},
	{reply, ok, InitState};

handle_call(sense, _, State) ->
	#state{dataset = Dataset, dataset_actions = DatasetActions, num_line_readed = NumReadedLines} = State,
	#dataset_actions{read_action = ReadFun, parse_line_action = ParseLineAction, extract_line_action = ExtractLineFun} = DatasetActions,
	%%Read next record in the dataset
	Record = ReadFun(Dataset, NumReadedLines + 1),
	%Parse the record
	ParsedRecord = ParseLineAction(Record),
	%Extract features and target from the parsed record
	{Features, _ } = ExtractLineFun(ParsedRecord),
	NewState = State#state{current = Record},
	{reply, Features, NewState};

handle_call({action_fit, Prediction},_,State) ->
	#state{current = Record, has_header = HasHeader, num_line_readed = NumLineReaded, limit = Limit, info = MapInfo, 
		   dataset_actions = DatasetActions, dataset = Dataset, matrix = Matrix, loss = LossAcc} = State,
	#dataset_actions{read_action = ReadFun, parse_line_action = ParseLineFun, extract_line_action = ExtractLineFun, 
					is_finished_action = IsFinishedFun, reset_action = ResetFun} = DatasetActions,
	%Parse the current readed record
	ParsedRecord = ParseLineFun(Record),
	%Extract target from the parsed record
	{_, Target} = ExtractLineFun(ParsedRecord),
	#{encoding := Encoding, len := Len, classes := Classes} = MapInfo,
	Encode = preprocess:encode(Target, Encoding),
	PartialLoss = ?METRICS:cross_entropy(Encode, Prediction),
	PartialFit = 1 - ?METRICS:manhattan_avg(Encode, Prediction),
	ClassChoose = preprocess:decode(preprocess:mostLikely(Prediction), Encoding),
	UpdateMatrix = ?METRICS:incr_cell_matrix(Target, ClassChoose, Matrix),
	case IsFinishedFun(Dataset, NumLineReaded) of
		true ->
			Fitness = ?METRICS:f1_score_avg(Classes, UpdateMatrix),
			Loss = (PartialLoss + LossAcc) / Len,
			NewMatrix = ?METRICS:create_matrix_confusion(erlang:map_get(classes, MapInfo)),
			%Reset the dataset
			InitialDataset = ResetFun(Dataset, NumLineReaded),
			%Drop header if the dataset have an header
			StartLineReaded = scape_utils:drop_header(HasHeader, InitialDataset, ReadFun),
			NewState = State#state{dataset = InitialDataset, num_line_readed = StartLineReaded, matrix = NewMatrix, loss = 0},
			Msg = #{type => classification, partial_fit => PartialFit, partial_loss => PartialLoss,
					loss => Loss, fitness => Fitness, target => Encode, predict => Prediction},
			{reply, {finish, Msg}, NewState};
		false -> %% must evaluate if i reached the limit of readable records
			case NumLineReaded == Limit of
				true->%stop the dataset fit reading
					Fitness = ?METRICS:f1_score_avg(Classes, UpdateMatrix),
					Loss = (PartialLoss + LossAcc) / Len,
					NewMatrix = ?METRICS:create_matrix_confusion(erlang:map_get(classes, MapInfo)),
					NewState=State#state{num_line_readed = 0, matrix = NewMatrix, loss = 0},
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

handle_call({action_fit_predict,Predict},_,State) ->
	#state{current = Record, has_header = HasHeader, dataset_actions = DatasetActions, dataset=Dataset, 
		  num_line_readed = NumLineReaded}=State,
	#dataset_actions{read_action = ReadFun, parse_line_action = ParseLineFun, extract_line_action = ExtractLineFun, 
					is_finished_action = IsFinishedFun, reset_action = ResetFun} = DatasetActions,
	%Parse the current readed record
	ParsedRecord = ParseLineFun(Record),
	%Extract target from the parsed record
	{_, Target} = ExtractLineFun(ParsedRecord),
	Msg = #{type => classification, target => Target, predict => Predict},
	case IsFinishedFun(Dataset, NumLineReaded) of
		true ->
			% Reset the dataset
			InitialDataset = ResetFun(Dataset, NumLineReaded),
			% Drop header if the dataset have an header
			StartLineReaded = scape_utils:drop_header(HasHeader, InitialDataset, ReadFun),
			NewState = State#state{dataset = InitialDataset, num_line_readed = StartLineReaded},
			{reply, {finish,Msg}, NewState};
		false ->
			NewState = State#state{num_line_readed = NumLineReaded + 1},
			{reply, {another,Msg}, NewState}
	end;

handle_call({action_predict, _}, _, State) ->
	{reply, ok, State}.

%%Assume that the State is the scape initial state!
extract_info(State) ->
	#state{dataset = Dataset, has_header = HasHeader, dataset_actions = DatasetActions, num_line_readed = NumLineReaded } = State,
	#dataset_actions{get_line_action = GetLineAction, extract_line_action = ExtractLineFun, 
					read_action = ReadFun, parse_line_action= ParseLineFun, reset_action = ResetFun} = DatasetActions,
	% 1) Get the first line of the dataset
	CurrentLine = GetLineAction(Dataset, NumLineReaded),
	% 2) Get the number of features
	ParsedLine = ParseLineFun(CurrentLine),
	{Features, _} = ExtractLineFun(ParsedLine),
	NumFeatures = length(Features),
	% 3) Initialize the mins and maxs vectors
	Mins = Maxs = lists:duplicate(NumFeatures, none),
	% 4) Initialize supports vectors for infos computation
	Sums = Scarti = lists:duplicate(NumFeatures, 0),
	% 5) Computes infos vectors
	% 5.1) Extract the first part of informations
	{NewMins, NewMaxs, NewSums, DatasetLen} = scape_utils:extract_part_one(State, Mins, Maxs, Sums),
	% 5.2) Reset the dataset
	ResetFun(Dataset, DatasetLen),
	StartLineReaded = scape_utils:drop_header(HasHeader, Dataset, ReadFun),
	NewState = State#state{num_line_readed = StartLineReaded},
	% 5.3) Extract the second part of informations
	Avgs = [Sum / DatasetLen || Sum <- NewSums],
	Stds = scape_utils:extract_part_two(NewState, Avgs, Scarti, DatasetLen),
	% 5.4) Reset the dataset
	ResetFun(Dataset, DatasetLen),
	StartLineReaded = scape_utils:drop_header(HasHeader, Dataset, ReadFun),
	NewState = State#state{num_line_readed = StartLineReaded},
	% 5.5) Extract information specifics for the classification
	Targets = extract_targets(NewState),
	Encoding = preprocess:one_hot(Targets),
	NumClasses = length(Targets),
	% 5.6) Reset the dataset
	ResetFun(Dataset, DatasetLen),
	StartLineReaded = scape_utils:drop_header(HasHeader, Dataset, ReadFun),
	NewState = State#state{num_line_readed = StartLineReaded},
	#{mins => NewMins, maxs => NewMaxs, len => DatasetLen, num_features => NumFeatures, num_classes => 
		NumClasses, classes => Targets, avgs => Avgs, stds => Stds, encoding => Encoding}.

extract_targets(State) ->
	extract_targets(State, []).
extract_targets(State, Targets) ->
	#state{dataset = Dataset, dataset_actions = DatasetActions, num_line_readed = NumLineReaded} = State,
	#dataset_actions{read_action = ReadFun, parse_line_action = ParseLineFun, 
					extract_line_action = ExtractLineFun, is_finished_action = IsFinishedFun} = DatasetActions,
	case IsFinishedFun(Dataset, NumLineReaded) of
		true -> 
			Targets;
		false ->
			Line = ReadFun(Dataset, NumLineReaded),
			ParsedLine = ParseLineFun(Line),
			{_, Target} = ExtractLineFun(ParsedLine),
			NewTargets = case lists:member(Target, Targets) of
							true -> Targets;
							false -> Targets ++ [Target]
						end,
			NewState = State#state{num_line_readed = NumLineReaded + 1},
			extract_targets(NewState, NewTargets)
	end.