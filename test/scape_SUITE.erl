-module(scape_SUITE).
-export([all/0]).
-export([classification_csv/1, classification_list/1, regression_csv/1, regression_list/1, 
        unsup_csv_labelled/1, unsup_csv_unlabelled/1, unsup_list_labelled/1, unsup_list_unlabelled/1]).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("Scape/scape.hrl").

all() -> [classification_csv, classification_list, regression_csv, regression_list, 
            unsup_csv_labelled, unsup_csv_unlabelled, unsup_list_labelled, unsup_list_unlabelled].


classification_csv(_) ->
    Dataset = "/usr/local/lib/python3.7/site-packages/sklearn/datasets/data/iris.csv",
    {ok, File} = file:open(Dataset, [read, binary]),
    DatasetActions = scape:get_dataset_actions(csv),
    ?assert(is_record(DatasetActions, dataset_actions)),
    {ok, Pid} = scape:start(classification, File, true, true, null, DatasetActions),
	Info = scape:extract_info(Pid),
	io:fwrite("INFO CLASSIFICATION: ~p~n",[Info]).

classification_list(_) ->
    Xor = [ [0,0,1],
            [0,1,0],
            [1,0,0],
            [1,1,1]
          ],
    DatasetActions = scape:get_dataset_actions(list),
    ?assert(is_record(DatasetActions, dataset_actions)),
    {ok, Pid} = scape:start(classification, Xor, false, true, 1, DatasetActions),
	Info = scape:extract_info(Pid),
    io:fwrite("INFO CLASSIFICATION: ~p~n",[Info]).

regression_csv(_) ->
    Dataset = code:priv_dir(neuralNet) ++ "/boston_house_prices.csv",
    {ok, File} = file:open(Dataset, [read,binary]),
    DatasetActions = scape:get_dataset_actions(csv),
    ?assert(is_record(DatasetActions, dataset_actions)),
    {ok, Pid} = scape:start(regression, File, true, true, null, DatasetActions),
	Info = scape:extract_info(Pid),
	io:fwrite("INFO REGRESSION: ~p~n",[Info]).

regression_list(_) ->
    Xor = [ [0,0,1],
            [0,1,0],
            [1,0,0],
            [1,1,1]
          ],
    DatasetActions = scape:get_dataset_actions(list),
    ?assert(is_record(DatasetActions, dataset_actions)),
    {ok, Pid} = scape:start(regression, Xor, false, true, 1, DatasetActions),
	Info = scape:extract_info(Pid),
    io:fwrite("INFO REGRESSION: ~p~n",[Info]).

unsup_csv_unlabelled(_) ->
    Dataset = code:priv_dir(neuralNet) ++ "/boston_house_prices.csv",
    {ok, File} = file:open(Dataset, [read,binary]),
    DatasetActions = scape:get_dataset_actions(csv),
    ?assert(is_record(DatasetActions, dataset_actions)),
    {ok, Pid} = scape:start(unsupervised, File, true, false, null, DatasetActions),
	Info = scape:extract_info(Pid),
	io:fwrite("INFO UNSUP UNLABELLED: ~p~n",[Info]).

unsup_list_unlabelled(_) ->
    Xor = [ [0,0,1],
            [0,1,0],
            [1,0,0],
            [1,1,1]
          ],
    DatasetActions = scape:get_dataset_actions(list),
    ?assert(is_record(DatasetActions, dataset_actions)),
    {ok, Pid} = scape:start(regression, Xor, false, false, 1, DatasetActions),
	Info = scape:extract_info(Pid),
    io:fwrite("INFO REGRESSION: ~p~n",[Info]).

unsup_csv_labelled(_) ->
    Dataset = code:priv_dir(neuralNet) ++ "/boston_house_prices.csv",
    {ok, File} = file:open(Dataset, [read,binary]),
    DatasetActions = scape:get_dataset_actions(csv),
    ?assert(is_record(DatasetActions, dataset_actions)),
    {ok, Pid} = scape:start(unsupervised, File, true, true, null, DatasetActions),
	Info = scape:extract_info(Pid),
	io:fwrite("INFO UNSUP LABELLED: ~p~n",[Info]).

unsup_list_labelled(_) ->
    Xor = [ [0,0,1],
            [0,1,0],
            [1,0,0],
            [1,1,1]
          ],
    DatasetActions = scape:get_dataset_actions(list),
    ?assert(is_record(DatasetActions, dataset_actions)),
    {ok, Pid} = scape:start(regression, Xor, false, true, 1, DatasetActions),
	Info = scape:extract_info(Pid),
    io:fwrite("INFO UNSUP LABELLED: ~p~n",[Info]).

