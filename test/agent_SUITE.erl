-module(agent_SUITE).
-export([all/0]).
-export([som/1, regressor_shc/1, classifier_shc/1, regressor_eshc/1, classifier_eshc/1]).
-include_lib("common_test/include/ct.hrl").

all() -> [som].

som(_)->
	Dataset = "/home/luca/.local/lib/python3.9/site-packages/sklearn/datasets/data/iris.csv",
	{ok, Pid} = unsupervised:start(Dataset, fun csv:read/1, labelled),
	Info = unsupervised:extract_info(Pid),
	#{mins := _, maxs := _, avgs := _, stds := _, num_features := FeatNum, len := Len} = Info,
	SensorSpec = {FeatNum, [], []},
	ActuatorSpec = {[], [{utils, get_BMU_cluster, []}]},
	CortexSpec = {[{fun(List) -> hd(List) end,[]}], [{fun(List) -> hd(List) end,[]}]},
	unsupervised:set_limit(Pid, Len * 60 / 100),
	nn:new(contr, Pid, {som, euclidean}, {SensorSpec, ActuatorSpec, CortexSpec, {7, 7}}),
	Fitness = nn:fit(contr, #{type => som, cycle => 300, learnRate => 3, neighboorSize => 3, num_clusters => 3, k_iterations => 100}, sync),
	ct:print(default, ?STD_IMPORTANCE, "FITNESS :~p",[Fitness]),
	nn:fit_predict(contr),
	nn:predict(contr,[5.5,4.2,1.4,0.2]),
	nn:predict(contr,[6.4,3.2,4.5,1.5]).

regressor_shc(_) ->
	Dataset = "/home/luca/.local/lib/python3.9/site-packages/sklearn/datasets/data/boston_house_prices.csv",
	{ok, Pid} = regression:start(Dataset, fun csv:read/1),
	Info = regression:extract_info(Pid),
	#{mins := Mins, maxs := Maxs, avgs := Avgs, stds := Stds, num_features := FeatNum, len := Len} = Info,
	SensorSpec = {FeatNum, [{preprocess, standardization_global, [Avgs, Stds]}], [{preprocess, standardization_global, [Avgs, Stds]}]},
	ActuatorSpec = {1,[{utils, actuator_get_signals, []}], [{utils, actuator_get_signals, []}]},
	CortexSpec = {[{fun(List) -> hd(List) end, []}], [{fun(List) -> hd(List) end, []}]},
	regression:set_limit(Pid, Len * 80 / 100),
	nn:new(contr, Pid, {ffnn, rectifier, none}, {SensorSpec, ActuatorSpec, CortexSpec, [round(FeatNum / 10)]}),
	Fitness = nn:fit(contr, #{type => shc, cycleShc => 100, stepnessNeuron => 40, stepnessWeight => 40, tgFit => 0.80}, sync),
	ct:print(default, ?STD_IMPORTANCE, "FITNESS :~p",[Fitness]),
	nn:fit_predict(contr).

classifier_shc(_) ->
	Dataset = "/home/luca/.local/lib/python3.9/site-packages/sklearn/datasets/data/iris.csv",
	{ok, File} = file:open(Dataset, [read, binary]),
    DatasetActions = scape:get_dataset_actions(csv),
    {ok, Pid} = scape:start(classification, File, true, true, null, DatasetActions),
	Info = scape:extract_info(Pid),
	#{mins := Mins, maxs := Maxs, avgs := Avgs, stds := Stds, num_features := FeatNum, num_classes := Classes, len := Len, encoding := Enc} = Info,
	SensorSpec = {FeatNum, [{data_processing, standardization_global, [Avgs, Stds]}], [{data_processing, standardization_global, [Avgs, Stds]}]},
	ActuatorSpec = {Classes, [{som_service, actuator_get_signals, []}, {data_processing, softmax,[]}], [{som_service, actuator_get_signals, []}, {data_processing, softmax, []}, {data_processing, most_likely, []}, {data_processing, decode,[Enc]}]},
	CortexSpec = {[{erlang, hd, []}], [{erlang, hd, []}]},
	scape:set_limit(Pid, Len * 85 / 100),
	nn:new(contr, Pid, {rnn, sigmund, none}, {SensorSpec, ActuatorSpec, CortexSpec, [4]}),
	Fitness = nn:fit(contr, #{type => shc, cycleShc => 300, stepnessNeuron => 40, stepnessWeight => 40, tgFit => 0.80}, sync),
	ct:print(default, ?STD_IMPORTANCE, "FITNESS :~p",[Fitness]),
	nn:fit_predict(contr).

regressor_eshc(_) ->
	Dataset = "/home/luca/.local/lib/python3.9/site-packages/sklearn/datasets/data/boston_house_prices.csv",
	{ok, Pid} = regression:start(Dataset, fun csv:read/1),
	Info = regression:extract_info(Pid),
	#{mins := Mins, maxs := Maxs, avgs := Avgs, stds := Stds, num_features := FeatNum, len := Len} = Info,
	SensorSpec = {FeatNum, [{preprocess, standardization_global, [Avgs, Stds]}], [{preprocess, standardization_global, [Avgs, Stds]}]},
	ActuatorSpec = {1,[{utils, actuator_get_signals, []}], [{utils, actuator_get_signals, []}]},
	CortexSpec = {[{fun(List) -> hd(List) end, []}], [{fun(List) -> hd(List) end, []}]},
	regression:set_limit(Pid, Len * 80 / 100),
	nn:new(contr, Pid, {ffnn, rectifier, none}, {SensorSpec, ActuatorSpec, CortexSpec, [round(FeatNum / 10)]}),
	Constraint = {none, [{af, [sigmund]}, {plast, none}]},
	Fitness = nn:fit(contr, #{type => eshc, mutations => 2, stepnessNeuron => 50, stepnessWeight => 50, constraint => Constraint, cycleEshc => 10, cycleShc => 100, tgFit => 0.80}, sync),
	ct:print(default, ?STD_IMPORTANCE, "FITNESS :~p",[Fitness]),
	nn:fit_predict(contr).

classifier_eshc(_) ->
	Dataset = "/home/luca/.local/lib/python3.9/site-packages/sklearn/datasets/data/iris.csv",
	{ok, File} = file:open(Dataset, [read, binary]),
    DatasetActions = scape:get_dataset_actions(csv),
    {ok, Pid} = scape:start(classification, File, true, true, null, DatasetActions),
	Info = scape:extract_info(Pid),
	#{mins := Mins, maxs := Maxs, avgs := Avgs, stds := Stds, num_features := FeatNum, num_classes := Classes, len := Len, encoding := Enc} = Info,
	SensorSpec = {FeatNum, [{data_processing, standardization_global, [Avgs, Stds]}], [{data_processing, standardization_global, [Avgs, Stds]}]},
	ActuatorSpec = {Classes, [{som_service, actuator_get_signals, []}, {data_processing, softmax,[]}], [{som_service, actuator_get_signals, []}, {data_processing, softmax, []}, {data_processing, most_likely, []}, {data_processing, decode,[Enc]}]},
	CortexSpec = {[{erlang, hd, []}], [{erlang, hd, []}]},
	scape:set_limit(Pid, Len * 85 / 100),
	nn:new(contr, Pid, {rnn, sigmund, none}, {SensorSpec, ActuatorSpec, CortexSpec, [4]}),
	Constraint = {[add_neuro_link, add_layer_neuron, add_neuron], [{af, [sigmund]}, {plast, none}]},
	Fitness = nn:fit(contr,#{type=>eshc,mutations=>5,stepnessNeuron=>50,stepnessWeight=>50,constraint=>Constraint,cycleEshc=>20,cycleShc=>100,tgFit=>1.0},sync),
	ct:print(default, ?STD_IMPORTANCE, "FITNESS :~p",[Fitness]),
	nn:fit_predict(contr).