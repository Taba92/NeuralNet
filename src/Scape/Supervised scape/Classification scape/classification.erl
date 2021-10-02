-module(classification).
-export([extract_info/1, init/1, handle_call/3]).
-record(state,{dataset,	has_header, dataset_actions, current, num_line_readed, limit, info, matrix, loss}).
-define(METRICS, classification_metrics).
-include("utils.hrl").
-include("Scape/scape.hrl").

init([Dataset, HasHeader, DatasetActions]) -> 
	#dataset_actions{read_action = ReadFun} = DatasetActions,
	%Drop the header if the dataset have one
	StartLineReaded = scape:drop_header(HasHeader, Dataset, ReadFun),
	State = #state{dataset = Dataset, has_header = HasHeader, dataset_actions = DatasetActions, num_line_readed = StartLineReaded, loss = 0},
	{ok, State}.

handle_call(extract_info, _, State) ->
	MapInfo = extract_info(State),
	Matrix = ?METRICS:create_matrix_confusion(erlang:map_get(classes, MapInfo)),
	{reply, MapInfo, State#state{info = MapInfo, matrix = Matrix }};

handle_call({set_limit,Limit}, _, State) ->
	{reply, ok, State#state{limit = round(Limit) }};

handle_call(reset, _, State) ->
	#state{has_header = HasHeader, dataset_actions = DatasetActions, dataset = Dataset, info = MapInfo} = State,
	#dataset_actions{read_action = ReadFun, reset_action = ResetFun} = DatasetActions,
	%Reset the dataset
	ResettedDataset = ResetFun(Dataset),
	%Recreate a new confusion matrix
	Matrix = ?METRICS:create_matrix_confusion(erlang:map_get(classes,MapInfo)),
	% Drop the header if the dataset have one
	StartLineReaded = scape:drop_header(HasHeader, ResettedDataset, ReadFun),
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
	case IsFinishedFun(Dataset) of
		true ->
			Fitness = ?METRICS:f1_score_avg(Classes, UpdateMatrix),
			Loss = (PartialLoss + LossAcc) / Len,
			NewMatrix = ?METRICS:create_matrix_confusion(erlang:map_get(classes, MapInfo)),
			%Reset the dataset
			InitialDataset = ResetFun(Dataset, NumLineReaded),
			%Drop header if the dataset have an header
			StartLineReaded = scape:drop_header(HasHeader, InitialDataset, ReadFun),
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
	case IsFinishedFun(Dataset) of
		true ->
			InitialDataset = ResetFun(Dataset, NumLineReaded),
			%Drop header if the dataset have an header
			StartLineReaded = scape:drop_header(HasHeader, InitialDataset, ReadFun),
			scape:drop_header(InitialDataset, ReadFun),
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
	#state{dataset = Dataset, dataset_actions = DatasetActions, num_line_readed = NumLineReaded } = State,
	#dataset_actions{get_line_action = GetLineAction, extract_line_action = ExtractLineFun} = DatasetActions,
	CurrentLine = GetLineAction(Dataset, NumLineReaded),
	%%Get the number of features
	{Features, _} = ExtractLineFun(CurrentLine),
	NumFeatures = length(Features),
	Mins = Maxs = lists:duplicate(NumFeatures, none),
	Sums = Scarti = lists:duplicate(NumFeatures, 0),
	{NewMins, NewMaxs, NewSums, Targets, Len} = extract(?READ(Dataset),Dataset,Fun,Mins,Maxs,Sums,[],0),
	Avgs = [Sum / Len || Sum <- NewSums],
	Stds=extract_file(?READ(Dataset),Dataset,Fun,Avgs,Scarti,Len),
	Encoding=preprocess:one_hot(Targets),
	NumClasses=length(Targets),
	#{mins=>NewMins,maxs=>NewMaxs,len=>Len,num_features=>NumFeatures,num_classes=>NumClasses,classes=>Targets,avgs=>Avgs,stds=>Stds,encoding=>Encoding}.

extract(eof,_,_,NewMins,NewMaxs,NewSums,NewTargets,NewLen)->{NewMins,NewMaxs,NewSums,NewTargets,NewLen};
extract({ok,<<Line/binary>>},Dataset,Fun,Mins,Maxs,Sums,Targets,Len)->
	{Features,Target}=?EXTRACT(Fun(Line)),
	NewMins=extract_min(Features,Mins),
	NewMaxs=extract_max(Features,Maxs),
	NewSums=extract_sum(Features,Sums),
	NewTargets=extract_target(Target,Targets),
	NewLen=Len+1,
	extract_file(?READ(Dataset),Dataset,Fun,NewMins,NewMaxs,NewSums,NewTargets,NewLen).

extract_min(Signal,Mins)->extract_min(Signal,Mins,[]).
extract_min([],[],NewMins)->NewMins;
extract_min([H|T],[ActualMin|Mins],Acc)->
	case ActualMin==none orelse H<ActualMin of
		true->extract_min(T,Mins,Acc++[H]);
		false->extract_min(T,Mins,Acc++[ActualMin])
	end.

extract_max(Signal,Mins)->extract_max(Signal,Mins,[]).
extract_max([],[],NewMaxs)->NewMaxs;
extract_max([H|T],[ActualMax|Maxs],Acc)->
	case ActualMax==none orelse H>ActualMax of
		true->extract_max(T,Maxs,Acc++[H]);
		false->extract_max(T,Maxs,Acc++[ActualMax])
	end.

extract_sum(Signal,Sums)->extract_sum(Signal,Sums,[]).
extract_sum([],[],NewSums)->NewSums;
extract_sum([H|T],[ActualSum|Sums],Acc)->extract_sum(T,Sums,Acc++[H+ActualSum]).

extract_target(Target,Targets)->
	case lists:member(Target,Targets) of
		true->Targets;
		false->Targets++[Target]
	end.

extract_scarti(Signal,Avgs,Scarti)->extract_scarti(Signal,Avgs,Scarti,[]).
extract_scarti([],[],[],NewScarti)->NewScarti;
extract_scarti([H|T],[Avg|Avgs],[Scarto|Scarti],Acc)->extract_scarti(T,Avgs,Scarti,Acc++[math:pow((H-Avg),2)+Scarto]).

extract_stds(Scarti,Len)->extract_stds(Scarti,Len,[]).
extract_stds([],_,Stds)->Stds;
extract_stds([Scarto|Scarti],Len,Acc)->extract_stds(Scarti,Len,Acc++[math:sqrt(Scarto/Len)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extract(file,Fun,Dataset)->
	{ok,<<Line/binary>>}=?READ(Dataset),
	NumFeatures=get_num_features_file(Fun(Line)),
	Mins=Maxs=lists:duplicate(NumFeatures,none),
	Sums=Scarti=lists:duplicate(NumFeatures,0),
	{NewMins,NewMaxs,NewSums,Targets,Len}=extract_file(?READ(Dataset),Dataset,Fun,Mins,Maxs,Sums,[],0),
	Avgs=[Sum/Len||Sum<-NewSums],
	file:position(Dataset,bof),
	?READ(Dataset),
	Stds=extract_file(?READ(Dataset),Dataset,Fun,Avgs,Scarti,Len),
	Encoding=preprocess:one_hot(Targets),
	NumClasses=length(Targets),
	#{mins=>NewMins,maxs=>NewMaxs,len=>Len,num_features=>NumFeatures,num_classes=>NumClasses,classes=>Targets,avgs=>Avgs,stds=>Stds,encoding=>Encoding}.

extract_file(eof,_,_,NewMins,NewMaxs,NewSums,NewTargets,NewLen)->{NewMins,NewMaxs,NewSums,NewTargets,NewLen};
extract_file({ok,<<Line/binary>>},Dataset,Fun,Mins,Maxs,Sums,Targets,Len)->
	{Features,Target}=?EXTRACT(Fun(Line)),
	NewMins=extract_min(Features,Mins),
	NewMaxs=extract_max(Features,Maxs),
	NewSums=extract_sum(Features,Sums),
	NewTargets=extract_target(Target,Targets),
	NewLen=Len+1,
	extract_file(?READ(Dataset),Dataset,Fun,NewMins,NewMaxs,NewSums,NewTargets,NewLen).

extract_file(eof,_,_,_,NewScarti,Len)->extract_stds(NewScarti,Len);
extract_file({ok,<<Line/binary>>},Dataset,Fun,Avgs,Scarti,Len)->
	{Features,_}=?EXTRACT(Fun(Line)),
	NewScarti=extract_scarti(Features,Avgs,Scarti),
	extract_file(?READ(Dataset),Dataset,Fun,Avgs,NewScarti,Len).

get_num_features_file(Record)->length(Record)-1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%FOR LIST%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extract(list,Dataset)->
	NumFeatures=get_num_features_list(Dataset),
	Mins=Maxs=lists:duplicate(NumFeatures,none),
	Sums=Scarti=lists:duplicate(NumFeatures,0),
	{NewMins,NewMaxs,NewSums,Targets,Len}=extract_list(Dataset,Mins,Maxs,Sums,[],0),
	Avgs=[Sum/Len||Sum<-NewSums],
	Stds=extract_list(Dataset,Avgs,Scarti,Len),
	Encoding=preprocess:one_hot(Targets),
	NumClasses=length(Targets),
	#{mins=>NewMins,maxs=>NewMaxs,len=>Len,num_features=>NumFeatures,num_classes=>NumClasses,classes=>Targets,avgs=>Avgs,stds=>Stds,encoding=>Encoding}.

extract_list([],NewMins,NewMaxs,NewSums,NewTargets,NewLen)->{NewMins,NewMaxs,NewSums,NewTargets,NewLen};
extract_list([Record|Dataset],Mins,Maxs,Sums,Targets,Len)->
	{Features,Target}=?EXTRACT(Record),
	NewMins=extract_min(Features,Mins),
	NewMaxs=extract_max(Features,Maxs),
	NewSums=extract_sum(Features,Sums),
	NewTargets=extract_target(Target,Targets),
	NewLen=Len+1,
	extract_list(Dataset,NewMins,NewMaxs,NewSums,NewTargets,NewLen).

extract_list([],_,NewScarti,Len)->extract_stds(NewScarti,Len);
extract_list([Record|Dataset],Avgs,Scarti,Len)->
	{Features,_}=?EXTRACT(Record),
	NewScarti=extract_scarti(Features,Avgs,Scarti),
	extract_list(Dataset,Avgs,NewScarti,Len).

get_num_features_list([Record|_])->length(Record)-1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
