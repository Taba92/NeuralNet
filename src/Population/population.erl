-module(population).
-export([new/5,evolve/5,get_agents/1,get_best_agent/1]).
-define(POP,population_monitor).
-include("utils.hrl").

new(Name,N,Scape,Constraint,Morphology)->
	gen_server:start({local,Name},?POP,[Name],[]),
	gen_server:call(Name,{spawn,N,Scape,Constraint,Morphology},infinity).

evolve(Name,NumGeneration,Constraint,Cycle,TgFit)->gen_server:call(Name,{evolve,NumGeneration,Constraint,Cycle,TgFit},infinity).

get_agents(Name)->gen_server:call(Name,get_agents,infinity).

get_best_agent(Name)->gen_server:call(Name,get_best_agent,infinity).
	