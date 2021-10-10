-module(scape_service).
-export([drop_header/1, extract_features_and_target/2, reset_scape/1]).
-export([extract_part_one/4, extract_part_two/4]).
-export([extract_min/2, extract_max/2, extract_sum/2, extract_scarti/3, extract_standards_deviations/2]).
-include("Scape/scape.hrl").

drop_header(State) when State#state.has_header == false ->
    State;
drop_header(State) when State#state.has_header == true->
	#state{dataset = Dataset, dataset_actions = DatasetActions, cursor = Cursor} = State,
	#dataset_actions{read_action = ReadFun} = DatasetActions,
    {_, UpdatedCursor} = ReadFun(Dataset, Cursor),
    State#state{cursor = UpdatedCursor}.

% Reset the scape dataset and its cursor
reset_scape(State) ->
	#state{dataset = Dataset, dataset_actions = DatasetActions, cursor = Cursor} = State,
	#dataset_actions{reset_action = ResetFun} = DatasetActions,
	{InitialDataset, InitialCursor} = ResetFun(Dataset, Cursor),
	NewState = State#state{dataset = InitialDataset, cursor = InitialCursor, num_line_readed = 0},
	drop_header(NewState).

%Extract features and target from the passed Record in base the labelling of the record (true or false)
extract_features_and_target(true, Line) when is_list(Line) ->
	LineLength = length(Line),
	lists:split(LineLength - 1, Line);
extract_features_and_target(false, Line) when is_list(Line)->
	{Line, []}.

% Foreach row of the dataset, update mins, maxs, sums columns vectors and length of the dataset
extract_part_one(State, Mins, Maxs, Sums) ->
	#state{dataset = Dataset, labelled = Labelled, dataset_actions = DatasetActions, cursor = Cursor, num_line_readed = NumLineReaded} = State,
	#dataset_actions{read_action = ReadFun, parse_line_action = ParseLineFun, is_finished_action = IsFinishedFun} = DatasetActions,
	case IsFinishedFun(Dataset, Cursor) of
		true -> 
			{Mins, Maxs, Sums, NumLineReaded};
		false ->
			{Line, NewCursor} = ReadFun(Dataset,Cursor),
			ParsedLine = ParseLineFun(Line),
			{Features, _} = extract_features_and_target(Labelled, ParsedLine),
			NewMins = extract_min(Features, Mins),
			NewMaxs = extract_max(Features, Maxs),
			NewSums = extract_sum(Features, Sums),
			NewState = State#state{cursor = NewCursor, num_line_readed = NumLineReaded + 1},
			extract_part_one(NewState, NewMins, NewMaxs, NewSums)
	end.

% Foreach row of the dataset update scarti columns vector for the final computation of the standards deviations of columns
extract_part_two(State, Avgs, Scarti, DatasetLen) ->
	#state{dataset = Dataset, labelled = Labelled, dataset_actions = DatasetActions, cursor = Cursor, num_line_readed = NumLineReaded} = State,
	#dataset_actions{read_action = ReadFun, parse_line_action = ParseLineFun, is_finished_action = IsFinishedFun} = DatasetActions,
	case IsFinishedFun(Dataset, Cursor) of
		true -> 
			extract_standards_deviations(Scarti, DatasetLen);
		false ->
			{Line, NewCursor} = ReadFun(Dataset, Cursor),
			ParsedLine = ParseLineFun(Line),
			{Features, _} = extract_features_and_target(Labelled, ParsedLine),
			NewScarti = extract_scarti(Features,Avgs,Scarti),
			NewState = State#state{cursor = NewCursor, num_line_readed = NumLineReaded + 1},
			extract_part_two(NewState, Avgs, NewScarti, DatasetLen)
	end.

extract_min(Line, Mins) -> 
	extract_min(Line, Mins, []).
extract_min([], [], NewMins) -> NewMins;
extract_min([H | T], [ActualMin | Mins], Acc) ->
	case H < ActualMin of
		true -> extract_min(T, Mins, Acc ++ [H]);
		false -> extract_min(T, Mins, Acc ++ [ActualMin])
	end.

extract_max(Line, Maxs) -> 
	extract_max(Line, Maxs, []).
extract_max([], [], NewMaxs) -> NewMaxs;
extract_max([H | T], [ActualMax | Maxs], Acc) ->
	case H > ActualMax of
		true -> extract_max(T, Maxs, Acc ++ [H]);
		false-> extract_max(T, Maxs, Acc ++ [ActualMax])
	end.

extract_sum(Line, Sums) -> 
	extract_sum(Line, Sums, []).
extract_sum([], [], NewSums) -> NewSums;
extract_sum([H | T], [ActualSum | Sums],Acc) -> 
	extract_sum(T, Sums, Acc ++ [H + ActualSum]).

extract_scarti(Line, Avgs, Scarti) -> 
	extract_scarti(Line, Avgs, Scarti, []).
extract_scarti([], [], [],NewScarti) -> NewScarti;
extract_scarti([H | T], [Avg | Avgs], [Scarto | Scarti], Acc) ->
	extract_scarti(T, Avgs, Scarti, Acc ++ [math:pow((H - Avg), 2) + Scarto]).

extract_standards_deviations(Scarti, Len) -> 
	extract_standards_deviations(Scarti, Len, []).
extract_standards_deviations([], _, Stds) -> Stds;
extract_standards_deviations([Scarto | Scarti], Len, Acc) ->
	extract_standards_deviations(Scarti, Len, Acc ++ [math:sqrt(Scarto / Len)]).