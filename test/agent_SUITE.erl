-module(agent_SUITE).
-export([all/0]).
-export([som/1, regressor/1, classifier/1]).
-include_lib("common_test/include/ct.hrl").

all() -> [som, regressor, classifier].



som(_)->
	%Dataset="boston_house_prices.csv",
	Dataset="/usr/local/lib/python3.7/site-packages/sklearn/datasets/data/iris.csv",
	{ok,Pid}=unsupervised:start(Dataset,fun csv:read/1,labelled),
	Info=unsupervised:extract_info(Pid),
	io:fwrite("INFO: ~p~n",[Info]),
	#{mins:=Mins,maxs:=Maxs,avgs:=Avgs,stds:=Stds,num_features:=FeatNum,len:=Len}=Info,
	%SensorSpec={FeatNum,[{preprocess,standardization_global,[Avgs,Stds]}],[{preprocess,standardization_global,[Avgs,Stds]}]},
	SensorSpec={FeatNum,[],[]},
	ActuatorSpec={[],[{utils,get_BMU_cluster,[]}]},
	CortexSpec={[{fun(List)->hd(List) end,[]}],[{fun(List)->hd(List) end,[]}]},
	unsupervised:set_limit(Pid,Len*60/100),
	nn:new(contr,Pid,{som,euclidean},{SensorSpec,ActuatorSpec,CortexSpec,{7,7}}),
	io:fwrite("FITNESS: ~p~n",[nn:fit(contr,#{type=>som,cycle=>300,learnRate=>3,neighboorSize=>3,num_clusters=>3,k_iterations=>100},sync)]),
	%io:fwrite("AGENT: ~p~n",[nn:get(contr)]);
	nn:fit_predict(contr),
	nn:predict(contr,[5.5,4.2,1.4,0.2]),
	nn:predict(contr,[6.4,3.2,4.5,1.5]).

regressor(_) ->
	Dataset="boston_house_prices.csv",
	{ok,Pid}=regression:start(Dataset,fun csv:read/1),
	Info=regression:extract_info(Pid),
	io:fwrite("INFO: ~p~n",[Info]),
	#{mins:=Mins,maxs:=Maxs,avgs:=Avgs,stds:=Stds,num_features:=FeatNum,len:=Len}=Info,
	SensorSpec={FeatNum,[{preprocess,standardization_global,[Avgs,Stds]}],[{preprocess,standardization_global,[Avgs,Stds]}]},
	ActuatorSpec={1,[{utils,actuator_get_signals,[]}],[{utils,actuator_get_signals,[]}]},
	CortexSpec={[{fun(List)->hd(List) end,[]}],[{fun(List)->hd(List) end,[]}]},
	regression:set_limit(Pid,Len*80/100),
	nn:new(contr,Pid,{ffnn,rectifier,none},{SensorSpec,ActuatorSpec,CortexSpec,[round(FeatNum/10)]}),
	io:fwrite("FITNESS: ~p~n",[nn:fit(contr,#{type=>shc,cycleShc=>100,stepnessNeuron=>40,stepnessWeight=>40,tgFit=>0.80},sync)]),
	Constraint={none,[{af,[sigmund]},{plast,none}]},
	%io:fwrite("FITNESS: ~p~n",[nn:fit(contr,#{type=>eshc,mutations=>2,stepnessNeuron=>50,stepnessWeight=>50,constraint=>Constraint,cycleEshc=>10,cycleShc=>100,tgFit=>0.80},sync)]),
	%nn:fit(contr,#{type=>ashc,constraint=>{ffnn,sigmund,none},stepnessNeuron=>40,stepnessWeight=>40,cycleShc=>100,tgFit=>0.90,cycleAshc=>10},sync),
	%population:new(pop,1,{scape,init,[]},{ffnn,rectifier,hebbian},{2,1,[]}),
	%population:evolve(pop,20,Constraint,100,0.95).
	nn:fit_predict(contr).

classifier(_) ->
	Dataset="/usr/local/lib/python3.7/site-packages/sklearn/datasets/data/iris.csv",
	%Dataset="breast_cancer.csv",
	{ok,Pid}=classification:start(Dataset,fun csv:read/1),
	Info=classification:extract_info(Pid),
	%io:fwrite("INFO: ~p~n",[Info]),
	#{mins:=Mins,maxs:=Maxs,avgs:=Avgs,stds:=Stds,num_features:=FeatNum,num_classes:=Classes,len:=Len,encoding:=Enc}=Info,
	SensorSpec={FeatNum,[{preprocess,standardization_global,[Avgs,Stds]}],[{preprocess,standardization_global,[Avgs,Stds]}]},
	ActuatorSpec={Classes,[{utils,actuator_get_signals,[]},{preprocess,softmax,[]}],[{utils,actuator_get_signals,[]},{preprocess,softmax,[]},{preprocess,mostLikely,[]},{preprocess,decode,[Enc]}]},
	CortexSpec={[{erlang,hd,[]}],[{erlang,hd,[]}]},
	classification:set_limit(Pid,Len*85/100),
	nn:new(contr,Pid,{rnn,sigmund,none},{SensorSpec,ActuatorSpec,CortexSpec,[4]}),
	%io:fwrite("FITNESS: ~p~n",[nn:fit(contr,#{type=>shc,cycleShc=>300,stepnessNeuron=>40,stepnessWeight=>40,tgFit=>0.80},sync)]),
	Constraint={[add_neuro_link,add_layer_neuron,add_neuron],[{af,[sigmund]},{plast,none}]},
	io:fwrite("FITNESS: ~p~n",[nn:fit(contr,#{type=>eshc,mutations=>5,stepnessNeuron=>50,stepnessWeight=>50,constraint=>Constraint,cycleEshc=>20,cycleShc=>100,tgFit=>1.0},sync)]),
	%nn:fit(contr,#{type=>ashc,constraint=>{ffnn,sigmund,none},stepnessNeuron=>40,stepnessWeight=>40,cycleShc=>100,tgFit=>0.90,cycleAshc=>10},sync),
	%population:new(pop,1,{scape,init,[]},{ffnn,rectifier,hebbian},{2,1,[]}),
	%population:evolve(pop,20,Constraint,100,0.95).
	nn:fit_predict(contr).