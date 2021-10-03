-module(scape).
-export([start/4, extract_info/1, set_limit/2]).
-export([get_dataset_actions/1]).
-define(CLASSIFICATION, classification).
-define(REGRESSION, regression).
-define(UNSUPERVISED, unsupervised).
-include_lib("stdlib/include/assert.hrl").
-include("Scape/scape.hrl").

is_valid_scape_type(ScapeType) ->
    ScapeType == ?CLASSIFICATION orelse ScapeType == ?REGRESSION orelse ScapeType == ?UNSUPERVISED.

is_valid_dataset_actions(DatasetActions) ->
    #dataset_actions{read_action = ReadFun, get_line_action = GetLineFun, parse_line_action = ParseLineFun, extract_line_action = ExtractLineFun,
        is_finished_action = IsFinishedFun, reset_action = ResetFun} = DatasetActions,
    is_function(ReadFun) andalso is_function(ParseLineFun) andalso is_function(ParseLineFun) andalso is_function(GetLineFun)
    andalso is_function(ExtractLineFun) andalso is_function(IsFinishedFun) andalso is_function(ResetFun).

%%Start a scape of the given type with given actions set on the dataset
start(ScapeType, Dataset, HasHeader, DatasetActions) when is_atom(ScapeType),
                                                        is_boolean(HasHeader),
                                                        is_record(DatasetActions, dataset_actions) ->
    ?assert(is_valid_scape_type(ScapeType)),
    ?assert(is_valid_dataset_actions(DatasetActions)),
    gen_server:start(ScapeType, [Dataset, HasHeader, DatasetActions], [] ).

%%Extract information about the dataset
extract_info(ScapeId) when is_atom(ScapeId);is_pid(ScapeId)->
    gen_server:call(ScapeId,extract_info,infinity).

%%Set the limit of number of readed records from dataset
set_limit(ScapeId, Limit) when is_atom(ScapeId);is_pid(ScapeId),is_integer(Limit)->
    gen_server:call(ScapeId,{set_limit,Limit},infinity).

get_dataset_actions(csv) ->
    #dataset_actions{
        read_action = fun(Dataset, _) -> 
                        {ok, Line} = file:read_line(Dataset),
                        Line
                    end,
        get_line_action = fun(Dataset, _) ->
                            {ok, Line} = file:read_line(Dataset),
                            LineSize = byte_size(Line),
                            {ok,CurrentPosition} = file:position(Dataset, cur),
                            file:position(Dataset, CurrentPosition - LineSize),
                            Line
                        end,
        reset_action = fun(Dataset, _) ->
                            file:position(Dataset, bof),
                            Dataset
                        end,
        parse_line_action = fun(Line) ->
                                csv:read(Line)
                            end,
        extract_line_action = fun(Line) ->
                                LineLength = length(Line),
                                lists:split(LineLength - 1, Line)
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
        read_action = fun(Dataset, NumLineReaded) -> 
                        lists:nth(NumLineReaded, Dataset)
                    end,
        get_line_action = fun(Dataset, NumLineReaded) ->
                            lists:nth(NumLineReaded, Dataset)
                        end,
        reset_action = fun(Dataset, _) ->
                            Dataset
                        end,
        parse_line_action = fun(Line) ->
                               Line
                            end,
        extract_line_action = fun(Line) ->
                                LineLength = length(Line),
                                lists:split(LineLength - 1, Line)
                            end,
        is_finished_action = fun(Dataset, NumLineReaded) ->
                                length(Dataset) == NumLineReaded
                            end
    }.
