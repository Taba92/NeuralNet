-module(scape_utils).
-export([drop_header/3]).
-export([extract_part_one/4, extract_part_two/4]).
-export([extract_min/2, extract_max/2, extract_sum/2, extract_scarti/3, extract_standards_deviations/2]).
-include("Scape/scape.hrl").

drop_header(false, _, _) ->
    1;
drop_header(true, Dataset, ReadFun) when is_function(ReadFun) ->
    ReadFun(Dataset, 1),
    2.

% Foreach row of the dataset, update mins, maxs, sums columns vectors and length of the dataset
extract_part_one(State, Mins, Maxs, Sums) ->
	#state{dataset = Dataset, dataset_actions = DatasetActions, num_line_readed = NumLineReaded} = State,
	#dataset_actions{read_action = ReadFun, parse_line_action = ParseLineFun, 
					extract_line_action = ExtractLineFun, is_finished_action = IsFinishedFun} = DatasetActions,
	case IsFinishedFun(Dataset, NumLineReaded) of
		true -> 
			{Mins, Maxs, Sums, NumLineReaded};
		false ->
			Line = ReadFun(Dataset, NumLineReaded),
			ParsedLine = ParseLineFun(Line),
			{Features, _} = ExtractLineFun(ParsedLine),
			NewMins = scape_utils:extract_min(Features, Mins),
			NewMaxs = scape_utils:extract_max(Features, Maxs),
			NewSums = scape_utils:extract_sum(Features, Sums),
			NewState = State#state{num_line_readed = NumLineReaded + 1},
			extract_part_one(NewState, NewMins, NewMaxs, NewSums)
	end.

% Foreach row of the dataset update scarti columns vector for the final computation of the standards deviations of columns
extract_part_two(State, Avgs, Scarti, DatasetLen) ->
	#state{dataset = Dataset, dataset_actions = DatasetActions, num_line_readed = NumLineReaded} = State,
	#dataset_actions{read_action = ReadFun, parse_line_action = ParseLineFun, 
					extract_line_action = ExtractLineFun, is_finished_action = IsFinishedFun} = DatasetActions,
	case IsFinishedFun(Dataset, NumLineReaded) of
		true -> 
			scape_utils:extract_standards_deviations(Scarti, DatasetLen);
		false ->
			Line = ReadFun(Dataset, NumLineReaded),
			ParsedLine = ParseLineFun(Line),
			{Features, _} = ExtractLineFun(ParsedLine),
			NewScarti = scape_utils:extract_scarti(Features,Avgs,Scarti),
			NewState = State#state{num_line_readed = NumLineReaded + 1},
			extract_part_two(NewState, Avgs, NewScarti, DatasetLen)
	end.

extract_min(Line, Mins) -> 
	extract_min(Line, Mins, []).
extract_min([], [], NewMins) -> NewMins;
extract_min([H | T], [ActualMin | Mins], Acc) ->
	case ActualMin == none orelse H < ActualMin of
		true -> extract_min(T, Mins, Acc ++ [H]);
		false -> extract_min(T, Mins, Acc ++ [ActualMin])
	end.

extract_max(Line, Maxs) -> 
	extract_max(Line, Maxs, []).
extract_max([], [], NewMaxs) -> NewMaxs;
extract_max([H | T], [ActualMax | Maxs], Acc) ->
	case ActualMax == none orelse H > ActualMax of
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