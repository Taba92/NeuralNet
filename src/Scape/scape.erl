-module(scape).
-export([start/6, extract_info/1, set_limit/2]).
-export([get_dataset_actions/1]).
-define(CLASSIFICATION, classification).
-define(REGRESSION, regression).
-define(UNSUPERVISED, unsupervised).
-include_lib("stdlib/include/assert.hrl").
-include("Scape/scape.hrl").

is_valid_scape_type(ScapeType) ->
    ScapeType == ?CLASSIFICATION orelse ScapeType == ?REGRESSION orelse ScapeType == ?UNSUPERVISED.

is_valid_dataset_actions(DatasetActions) ->
    #dataset_actions{read_action = ReadFun, parse_line_action = ParseLineFun,
        is_finished_action = IsFinishedFun, reset_action = ResetFun} = DatasetActions,
    is_function(ReadFun) andalso is_function(ParseLineFun) 
    andalso is_function(IsFinishedFun) andalso is_function(ResetFun).

%%Start a scape of the given type with given actions set on the dataset
start(ScapeType, Dataset, HasHeader, Labelled, Cursor, DatasetActions) when is_atom(ScapeType),
                                                        is_boolean(HasHeader),
                                                        is_boolean(Labelled),
                                                        is_record(DatasetActions, dataset_actions) ->
    ?assert(is_valid_scape_type(ScapeType)),
    ?assert(is_valid_dataset_actions(DatasetActions)),
    gen_server:start(ScapeType, [Dataset, HasHeader, Labelled, Cursor, DatasetActions], [] ).

%%Extract information about the dataset
extract_info(ScapeId) when is_atom(ScapeId);is_pid(ScapeId)->
    gen_server:call(ScapeId,extract_info,infinity).

%%Set the limit of number of readed records from dataset
set_limit(ScapeId, Limit) ->
    gen_server:call(ScapeId, {set_limit, Limit}, infinity).

get_dataset_actions(csv) ->
    #dataset_actions{
        read_action = fun(Dataset, _) -> 
                        {ok, Line} = file:read_line(Dataset),
                        {Line, undefined}
                    end,
        reset_action = fun(Dataset, _) ->
                            file:position(Dataset, bof),
                            {Dataset, undefined}
                        end,
        parse_line_action = fun(Line) ->
                                csv:read(Line)
                            end,
        is_finished_action = fun(Dataset, _) ->
                                {ok, CurrentPosition} = file:position(Dataset, cur),
                                {ok, EndOfFile} = file:position(Dataset, eof),
                                file:position(Dataset, CurrentPosition),
                                CurrentPosition == EndOfFile
                            end
    };
get_dataset_actions(list) ->
        #dataset_actions{
        read_action = fun(Dataset, CurrentLineNumber) -> 
                        {lists:nth(CurrentLineNumber, Dataset), CurrentLineNumber + 1}
                    end,
        reset_action = fun(Dataset, _) ->
                            {Dataset, 1}
                        end,
        parse_line_action = fun(Line) ->
                               Line
                            end,
        is_finished_action = fun(Dataset, CurrentLineNumber) ->
                                length(Dataset) == CurrentLineNumber - 1
                            end
    }.
