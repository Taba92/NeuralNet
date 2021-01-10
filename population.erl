-module(population).
-export([new/5,evolve/4]).
-define(POP,population_monitor).
-include("utils.hrl").

new(Name,N,Scape,Constraint,Morphology)->
	gen_server:start({local,Name},?POP,[Name],[]),
	gen_server:call(Name,{spawn,N,Scape,Constraint,Morphology},infinity).

evolve(Name,NumGeneration,Cycle,TgFit)->gen_server:call(Name,{evolve,NumGeneration,Cycle,TgFit},infinity).