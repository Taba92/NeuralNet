-module(genotype_SUITE).
-export([all/0]).
-export([create_som/1, create_classic/1]).
-include_lib("common_test/include/ct.hrl").

all() -> [create_som, create_classic].


create_som(_)->
    SensorSpec={3,[],[]},
	ActuatorSpec={[],[{utils,get_BMU_cluster,[]}]},
	CortexSpec={[{fun(List)->hd(List) end,[]}],[{fun(List)->hd(List) end,[]}]},
    genotype:create_NN({som,euclidean},{SensorSpec,ActuatorSpec,CortexSpec,{7,7}}),
	ok.

create_classic(_) ->
    Dataset="boston_house_prices.csv",
	{ok,Pid}=regression:start(Dataset,fun csv:read/1),
	Info=regression:extract_info(Pid),
	#{mins:=Mins,maxs:=Maxs,avgs:=Avgs,stds:=Stds,num_features:=FeatNum,len:=Len}=Info,
	SensorSpec={FeatNum,[{preprocess,standardization_global,[Avgs,Stds]}],[{preprocess,standardization_global,[Avgs,Stds]}]},
	ActuatorSpec={1,[{utils,actuator_get_signals,[]}],[{utils,actuator_get_signals,[]}]},
	CortexSpec={[{fun(List)->hd(List) end,[]}],[{fun(List)->hd(List) end,[]}]},
    genotype:create_NN({ffnn,rectifier,none},{SensorSpec,ActuatorSpec,CortexSpec,[round(FeatNum/10)]}),
	ok.