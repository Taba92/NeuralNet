-module(scape_SUITE).
-export([all/0]).
-export([classification_csv/1,classification_list/1]).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("Scape/scape.hrl").

all() -> [classification_csv,classification_list].


classification_csv(_) ->
    Dataset = "/usr/local/lib/python3.7/site-packages/sklearn/datasets/data/iris.csv",
    {ok, File} = file:open(Dataset, [read, binary]),
    DatasetActions = scape:get_dataset_actions(csv),
    ?assert(is_record(DatasetActions, dataset_actions)),
    {ok, Pid} = scape:start(classification,File,true, DatasetActions),
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
    {ok, Pid} = scape:start(classification, Xor, false, DatasetActions),
	Info = scape:extract_info(Pid),
	io:fwrite("INFO CLASSIFICATION: ~p~n",[Info]).

