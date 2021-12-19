-module(population_monitor).
-export([init/1,handle_call/3,terminate/2]).
-define(EFF,0.1).
-define(POP_LIMIT,20).
-include("utils.hrl").
-include("phenotype.hrl").
-include("genotype.hrl").
-record(selection,{agent,truefit,allotedOffspring,nao}).

init([Name])->
	State=#population{id=Name},
	{ok,State}.

terminate(normal,State)->
	[nn:stop(Agent)||Agent<-State#population.agents].

handle_call({spawn,N,{Mod,Fun,Args},Constraint,Morphology},_,State)->
	CreateAgent=fun(Id)->
				{ok,ScapeId}=erlang:apply(Mod,Fun,Args),
				nn:new(Id,ScapeId,Constraint,Morphology),
				Id end,
	AgentsId=[CreateAgent(?GETID)||_<-lists:seq(1,N)],
	{reply,ok,State#population{agents=AgentsId}};
handle_call({evolve,Generation,Constraint,Cycle,TgFit},_,State)->
	Params=#{type=>shc,cycleShc=>Cycle,tgFit=>TgFit},
	EvolutionParams=#{generation=>Generation,constraint=>Constraint,tgfit=>TgFit},
	NewState=evolution(State,Params,0,EvolutionParams),
	{reply,ok,NewState};
handle_call(get_agents,_,State)->[nn:get(Id,sync)||Id<-State#population.agents];
handle_call(get_best_agents,_,State)->
	Agents=[nn:get(Id,sync)||Id<-State#population.agents],
	A=fun(Agent,{MaxFit,CurAgent})->case Agent#agent.fitness>MaxFit of true->{Agent#agent.fitness,Agent};false->{MaxFit,CurAgent} end end,
	{_,BestAgent}=lists:foldl(A,{0,null},Agents),
	BestAgent.

evolution(State,Params,Generation,EvoParams)->
	#{generation:=Gen,constraint:=Constraint,tgfit:=TgFit}=EvoParams,
	[nn:fit(Agent,Params,async)||Agent<-State#population.agents],
	AgentsFitted=[nn:get(Agent,sync)||Agent<-State#population.agents],
	[nn:stop(Agent)||Agent<-State#population.agents],
	TrueFitted=[true_fit(Agent)||Agent<-AgentsFitted],
	#selection{truefit=BestFit}=lists:last(lists:keysort(3,TrueFitted)),%prendo il miglior fitness
	case Gen==Generation+1 orelse BestFit>=TgFit of
		true->io:fwrite("EVO:~p~n",[AgentsFitted]),State;
		false->
			Survivors=adaptation(TrueFitted),
			{NewPopulation,_}=lists:foldl(fun reproduction/2,{[],Constraint},Survivors),
			NewState=populate(State#population{agents=[]},NewPopulation),
			evolution(NewState,Params,Generation+1,EvoParams)
	end.

true_fit(Agent)->
	#agent{genotype=#genotype{neurons=Neurons},fitness=Fitness}=Agent,
	#selection{agent=Agent,truefit=Fitness/math:pow(length(Neurons),?EFF)}.

allotedOffspring(Selection,NeuronEnergyCost)->
	#selection{agent=Agent,truefit=TrueFit}=Selection,
	#agent{genotype=#genotype{neurons=Neurons}}=Agent,
	AllotedNeurons = (TrueFit/NeuronEnergyCost),
	AllotedOffsprings=round(AllotedNeurons/length(Neurons)),
	Selection#selection{allotedOffspring=AllotedOffsprings}.

nao(Selection,PopulationNormalizer)->
	#selection{allotedOffspring=Alloted}=Selection,
	Nao=round(Alloted/PopulationNormalizer),
	Selection#selection{nao=Nao}.

adaptation(TrueFitted)->
	TotEnergy=lists:sum([Fit||#selection{truefit=Fit}<-TrueFitted]),
	TotNeurons=lists:sum([length(Neurons)||#selection{agent=#agent{genotype=#genotype{neurons=Neurons}}}<-TrueFitted]),
	NeuronEnergyCost = TotEnergy/TotNeurons,
	SortedAgents=lists:keysort(3,TrueFitted),
	{Selected,_}=lists:split(round(length(TrueFitted)/2),SortedAgents),
	NewSelected=[allotedOffspring(S,NeuronEnergyCost)||S<-Selected],
	TotalNewOffsprings=lists:sum([A||#selection{allotedOffspring=A}<-NewSelected]),
	PopulationNormalizer= TotalNewOffsprings/?POP_LIMIT,
	[nao(Sel,PopulationNormalizer)||Sel<-NewSelected].

reproduction(Survived,{NewPopulation,Constraint})->
	#selection{agent=Agent,nao=Nao}=Survived,
	case Nao of
		0-> {NewPopulation,Constraint};
		N when N>0,N=<1->{[Agent|NewPopulation],Constraint};
		_->{[Agent]++create_offspring(Agent,Nao,Constraint)++NewPopulation,Constraint}
	end.

create_offspring(Agent,Nao,Constraint)->
	#agent{scape=Scape,genotype=Genotype}=Agent,
	[create_new_agent(Scape,Genotype,Constraint)||_<-lists:seq(1,Nao-1)].

create_new_agent(Scape,Genotype,Constraint)->
	#genotype{neurons=Neurons}=Genotype,
	NMutation=round(math:sqrt(length(Neurons))),
	Cloned=genotype_mutator:clone(Genotype),
	{EvolvedGeno, _} = genotype_mutator:mutate(Cloned,NMutation,Constraint),
	NewAgent=#agent{id=?GETID,scape=Scape,genotype=EvolvedGeno},
	NewAgent.

populate(NewState,[])->NewState;
populate(State,[H|RestPopulation])->
	#agent{id=Id,scape=ScapeId,genotype=Genotype}=H,
	nn:new(Id,Genotype),
	nn:set_scape(Id,ScapeId,sync),
	populate(State#population{agents=State#population.agents++[Id]},RestPopulation).