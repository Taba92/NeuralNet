-module(nn).
-export([new/2,new/3,new/4,fit/2,fit/3,predict/2,predict/3,get/1,get/2,save_nn_file/2,save_nn_file/3,load_nn_file/2,load_nn_file/3,
		stop/1,set_scape/2,set_scape/3,fit_predict/1,fit_predict/2]).
-define(CONTROLLER,agent).
-include("utils.hrl").

new(Name,Genotype)when is_record(Genotype,genotype),is_atom(Name)->
	new(Name,Genotype,0).
new(Name,Genotype,Fitness)when is_record(Genotype,genotype),is_atom(Name),is_number(Fitness)->
	gen_server:start({local,Name},?CONTROLLER,[Name,Genotype,Fitness],[]).
new(Name,ScapeId,Constraint,{SensorSpec,ActuatorSpec,CortexSpec,Params})->
	Genotype=genotype:create_NN(Constraint,SensorSpec,ActuatorSpec,CortexSpec,Params),
	new(Name,Genotype),
	set_scape(Name,ScapeId).

stop(AgentId)->gen_server:stop(AgentId,normal,infinity).

get(AgentId)->get(AgentId,sync).
get(AgentId,sync)->gen_server:call(AgentId,get,infinity);
get(AgentId,async)->gen_server:send_request(AgentId,get).

set_scape(AgentId,ScapeId)->set_scape(AgentId,ScapeId,sync).
set_scape(AgentId,ScapeId,sync)when is_atom(ScapeId),is_pid(ScapeId)->gen_server:call(AgentId,{set_scape,ScapeId},infinity);
set_scape(AgentId,ScapeId,async)when is_atom(ScapeId),is_pid(ScapeId)->gen_server:send_request(AgentId,{set_scape,ScapeId}).


fit_predict(AgentId)->fit_predict(AgentId,sync).
fit_predict(AgentId,sync)->gen_server:call(AgentId,fit_predict,infinity);
fit_predict(AgentId,async)->gen_server:send_request(AgentId,fit_predict,infinity).

fit(AgentId,Params)->fit(AgentId,Params,sync).
fit(AgentId,Params,sync)->gen_server:call(AgentId,{fit,Params},infinity);
fit(AgentId,Params,async)->gen_server:send_request(AgentId,{fit,Params}).

predict(AgentId,Signal)->predict(AgentId,Signal,sync).
predict(AgentId,Signal,sync)->gen_server:call(AgentId,{predict,Signal},infinity);
predict(AgentId,Signal,async)->gen_server:send_request(AgentId,{predict,Signal}).

save_nn_file(AgentId,FileName)->save_nn_file(AgentId,FileName,sync).
save_nn_file(AgentId,FileName,sync)->gen_server:call(AgentId,{save_nn,FileName},infinity);
save_nn_file(AgentId,FileName,async)->gen_server:send_request(AgentId,{save_nn,FileName}).

load_nn_file(AgentId,FileName)->load_nn_file(AgentId,FileName,sync).
load_nn_file(AgentId,FileName,sync)->gen_server:call(AgentId,{load_nn,FileName},infinity);
load_nn_file(AgentId,FileName,async)->gen_server:send_request(AgentId,{load_nn,FileName}).
