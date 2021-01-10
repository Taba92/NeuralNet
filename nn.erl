-module(nn).
-export([new/2,new/3,new/4,fit/2,predict/2,get/1,save_nn_file/2,load_nn_file/2,
		stop/1,set_scape/2]).
-define(CONTR,agent).
-include("utils.hrl").

new(Name,Genotype)when is_record(Genotype,genotype),is_atom(Name)->
	new(Name,Genotype,0).
new(Name,Genotype,Fitness)when is_record(Genotype,genotype),is_atom(Name),is_number(Fitness)->
	gen_server:start({local,Name},?CONTR,[Name,Genotype,Fitness],[]).
new(Name,ScapeId,Constraint,{SensorVl,ActuatorVl,HiddenLayerDensity})->
	Genotype=genotype:create_NN(Constraint,SensorVl,ActuatorVl,HiddenLayerDensity),
	new(Name,Genotype),
	set_scape(Name,ScapeId).

stop(AgentId)->gen_server:stop(AgentId,normal,infinity).

get(AgentId)->gen_server:call(AgentId,get).

set_scape(AgentId,ScapeId)->gen_server:call(AgentId,{set_scape,ScapeId}).

fit(AgentId,Params)->
	%Params=#{type=>shc,cycleShc=>100,tgFit=>0.90},
	%Params=#{type=>ashc,cycleShc=>100,tgFit=>0.90,cycleAshc=>100}, 
	gen_server:call(AgentId,{fit,Params},infinity).

predict(AgentId,Signal)->gen_server:call(AgentId,{predict,Signal}).

save_nn_file(AgentId,FileName)->gen_server:call(AgentId,{save_nn,FileName}).

load_nn_file(AgentId,FileName)->gen_server:call(AgentId,{save_nn,FileName}).

