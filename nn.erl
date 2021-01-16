-module(nn).
-export([new/2,new/3,new/4,fit/3,predict/3,predict_fwd/4,get/2,save_nn_file/3,load_nn_file/3,
		stop/1,set_scape/3]).
-define(CONTR,agent).
-include("utils.hrl").

new(Name,Genotype)when is_record(Genotype,genotype),is_atom(Name)->
	new(Name,Genotype,0).
new(Name,Genotype,Fitness)when is_record(Genotype,genotype),is_atom(Name),is_number(Fitness)->
	gen_server:start({local,Name},?CONTR,[Name,Genotype,Fitness],[]).
new(Name,ScapeId,Constraint,{SensorVl,ActuatorVl,HiddenLayerDensity})->
	Genotype=genotype:create_NN(Constraint,SensorVl,ActuatorVl,HiddenLayerDensity),
	new(Name,Genotype),
	set_scape(Name,ScapeId,sync).

stop(AgentId)->gen_server:stop(AgentId,normal,infinity).

get(AgentId,sync)->gen_server:call(AgentId,get,infinity);
get(AgentId,async)->gen_server:send_request(AgentId,get).

set_scape(AgentId,ScapeId,sync)when is_atom(ScapeId);is_pid(ScapeId)->gen_server:call(AgentId,{set_scape,ScapeId},infinity);
set_scape(AgentId,ScapeId,async)when is_atom(ScapeId);is_pid(ScapeId)->gen_server:send_request(AgentId,{set_scape,ScapeId}).

fit(AgentId,Params,sync)->gen_server:call(AgentId,{fit,Params},infinity);
fit(AgentId,Params,async)->gen_server:send_request(AgentId,{fit,Params}).

predict(AgentId,Signal,sync)->gen_server:call(AgentId,{predict,Signal},infinity);
predict(AgentId,Signal,async)->gen_server:send_request(AgentId,{predict,Signal}).

predict_fwd(AgentId,Signal,Pids,sync)->gen_server:call(AgentId,{predict_fwd,Signal,Pids},infinity);
predict_fwd(AgentId,Signal,Pids,async)->gen_server:send_request(AgentId,{predict_fwd,Signal,Pids}).

save_nn_file(AgentId,FileName,sync)->gen_server:call(AgentId,{save_nn,FileName},infinity);
save_nn_file(AgentId,FileName,async)->gen_server:send_request(AgentId,{save_nn,FileName}).

load_nn_file(AgentId,FileName,sync)->gen_server:call(AgentId,{load_nn,FileName},infinity);
load_nn_file(AgentId,FileName,async)->gen_server:send_request(AgentId,{load_nn,FileName}).
