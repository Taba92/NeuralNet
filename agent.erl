-module(agent).
-export([init/1,handle_call/3,terminate/2]).
-include("utils.hrl").

init([Id,Genotype,Fitness])->
	phenotype:geno_to_pheno(Genotype),
	CortexId=genotype:get_cortex_id(Genotype),
	State=phenotype:link_to_cortex(#agent{id=Id,genotype=Genotype,fitness=Fitness},CortexId),
	{ok,State}.

terminate(normal,State)->
	#agent{cortexId=CortexId}=State,
	phenotype:stop_phenotype(CortexId).

handle_call({save_nn,FileName},_,State)->
	#agent{genotype=Genotype,fitness=Fitness}=State,
	{ok,Dets}=dets:open_file(d,[{file,FileName},{type,set}]),
	dets:insert(Dets,[{fitness,Fitness},{genotype,Genotype}]),
	dets:close(Dets),
	{reply,ok,State};
handle_call({load_nn,FileName},_,State)->
	{ok,Dets}=dets:open_file(d,[{file,FileName},{type,set}]),
	[{fitness,Fitness}]=dets:lookup(Dets,fitness),
	[{genotype,Genotype}]=dets:lookup(Dets,genotype),
	dets:close(Dets),
	{reply,ok,State#agent{genotype=Genotype,fitness=Fitness}};
handle_call({set_scape,ScapeId},_,State)->
	#agent{genotype=Genotype}=State,
	phenotype:link_nn_to_scape(Genotype,ScapeId),
	{reply,ok,State#agent{scape=ScapeId}};
handle_call({set_directives,actuators,Type,Directives},_,State)->
	#agent{genotype=Genotype}=State,
	#genotype{actuators=Acts}=Genotype,
	[gen_server:call(Id,{set_directives,Type,Directives})||#actuator{id=Id}<-Acts],
	{reply,ok,State};
handle_call({set_directives,sensors,Type,Directives},_,State)->
	#agent{genotype=Genotype}=State,
	#genotype{sensors=Sensors}=Genotype,
	[gen_server:call(Id,{set_directives,Type,Directives})||#sensor{id=Id}<-Sensors],
	{reply,ok,State};
handle_call(fit_predict,_,State)->
	#agent{cortexId=CortexId}=State,
	CortexId ! fit_predict_cycle,
	receive fit_predict_finish->ok end,
	{reply,ok,State};
handle_call({predict,Signal},_,State)->
	#agent{cortexId=CortexId}=State,
	CortexId ! {predict_cycle,Signal},
	receive {prediction,Prediction}->ok end,
	{reply,Prediction,State};	
handle_call({fit,Params},_,State)->
	%io:fwrite("GENO FITTING: ~p~n",[State#agent.genotype]),
	NewState=trainer:fit(State,Params),
	{reply,NewState#agent.fitness,NewState};
handle_call(get,_,State)->
	{reply,State,State}.