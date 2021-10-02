-module(scape).
-export([start/4, extract_info/1, set_limit/2]).
-export([drop_header/3]).
-define(CLASSIFICATION, classification).
-define(REGRESSION, regression).
-define(UNSUPERVISED, unsupervised).
-include_lib("stdlib/include/assert.hrl").
-include("Scape/scape.hrl").

%%Scape utilities
% If the dataset have an header, it read and drop the header line e move the line cursor by one
drop_header(false, _, _) ->
    0;
drop_header(true, Dataset, ReadFun) when is_function(ReadFun) ->
    ReadFun(Dataset),
    1.

%%
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