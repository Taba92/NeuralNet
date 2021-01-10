-module(neuronPheno).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-export([perturbate/3]).
-record(state,{acc,received,roreceived,oldBias,oldWeights,oldRoWeights,genotype}).
-include("utils.hrl").

init(GenoType)when is_record(GenoType,neuron)->
	#neuron{id=Id}=GenoType,
	gen_server:start_link({local,Id},?MODULE,[GenoType],[]);
init([GenoType])->
	#neuron{roinsWeights=Ro}=GenoType,
	N=length(Ro),
	State=#state{acc=0,received=[],roreceived=lists:duplicate(N,[]),genotype=GenoType},
	{ok,State}.

handle_call(dump,_,State)->
	#state{genotype=GenoType}=State,
	{reply,GenoType,State};
handle_call(backup_weights,_,State)->
	#state{genotype=GenoType}=State,
	#neuron{bias=Bias,faninsWeights=Weights,roinsWeights=RoWeights}=GenoType,
	{reply,ok,State#state{oldBias=Bias,oldWeights=Weights,oldRoWeights=RoWeights}};
handle_call(restore_weights,_,State)->
	#state{oldBias=OldBias,oldWeights=OldWeights,oldRoWeights=OldRoWeights,genotype=GenoType}=State,
	NewGenotype=GenoType#neuron{bias=OldBias,faninsWeights=OldWeights,roinsWeights=OldRoWeights},
	{reply,ok,State#state{genotype=NewGenotype}};
handle_call({perturb_weights,NNSize},_,State)->
	#state{genotype=GenoType}=State,
	#neuron{bias=Bias,faninsWeights=Ins,roinsWeights=RoIns}=GenoType,
	NewState=case ?PROB(math:sqrt(NNSize)) of
				true->
					Sup=math:sqrt(length(Ins)+length(RoIns)),
					NewBias=perturbate(Bias,Sup),
					FanIns=[perturbate_weight(Weight,Sup)||Weight<-Ins],
					RoInsW=[perturbate_weight(RoWeight,Sup)||RoWeight<-RoIns],
					NewFanIns=[perturbate_plast(Weight,Sup)||Weight<-FanIns],
					NewRoIns=[perturbate_plast(RoWeight,Sup)||RoWeight<-RoInsW],
					NewGenoType=GenoType#neuron{bias=NewBias,faninsWeights=NewFanIns,roinsWeights=NewRoIns},
					State#state{genotype=NewGenoType};
				false->State
			end,
	{reply,ok,NewState}.

terminate(normal,_)->ok.

handle_cast({ElType,FromLayer,IdFrom,FwdType,Signal},State)when ElType==sensor;ElType==neuron->
	#state{acc=Acc,received=Recv,roreceived=RoRecv,genotype=GenoType}=State,
	#neuron{id=Id,layer=Layer,af=Af,bias=Bias,faninsWeights=Ins,fanouts=Outs,roinsWeights=RoIns,roouts=RoOuts}=GenoType,
	{NewRecv,NewRoRecv}=case FromLayer>=Layer of
								true->
									{IdFrom,Weights,_}=lists:keyfind(IdFrom,1,RoIns),
									{Recv,RoRecv++[{IdFrom,Signal}]};
								false->
									{IdFrom,Weights,_}=lists:keyfind(IdFrom,1,Ins),
									{Recv++[{IdFrom,Signal}],RoRecv}
								end,
	NewState=case length(NewRecv)+length(NewRoRecv)==length(Ins)+length(RoIns) of
		        	true->
		        		Dot=dot(Signal,Weights,Acc),
						OutPut=af:Af(Dot+Bias),
						[gen_server:cast(Pid,{neuron,Layer,Id,FwdType,[OutPut]})||Pid<-Outs++RoOuts],
						NewGenotype=learn(GenoType,[OutPut],NewRecv,NewRoRecv),
						State#state{acc=0,received=[],roreceived=[],genotype=NewGenotype};
					false->
						NewAcc=dot(Signal,Weights,Acc),
						State#state{acc=NewAcc,received=NewRecv,roreceived=NewRoRecv}
			end,
	%io:fwrite("NEW STATE: ~p~n",[NewState]),
	{noreply,NewState}.

perturbate_plast({Id,Weight,Mod},Sup)->
	case ?PROB(Sup) of
		true->{Id,Weight,plast_perturb(Mod,length(Weight))};
		false->{Id,Weight,Mod}
	end.

plast_perturb(none,_)->none;
plast_perturb({hebbian,Vals},Sup)->{hebbian,perturbate_vals(Vals,Sup)};
plast_perturb({oja,Vals},Sup)->{oja,perturbate_vals(Vals,Sup)};
plast_perturb({neuromod,N,Vals},Sup)->{neuromod,N,[perturbate_vals(Val,Sup)||Val<-Vals]}.

perturbate_weight({Id,Weight,Mod},Sup)->
	case ?PROB(Sup) of
		true->{Id,perturbate_vals(Weight,length(Weight)),Mod};
		false->{Id,Weight,Mod}
	end.

perturbate_vals([],_)->[];
perturbate_vals({L,B},Sup)->{perturbate_vals(L,Sup),perturbate(B,Sup)};
perturbate_vals([H|T],Sup)->
	[perturbate(H,Sup)|perturbate_vals(T,Sup)].

perturbate(Val,Sup)->
	case ?PROB(Sup) of
		true->perturbate(Val,-?SAT_LIMIT,?SAT_LIMIT);
		false->Val
	end.
perturbate(Val,Min,Max)->
	Perturbed=(?RAND)*?E+Val,
	if
		Perturbed < Min -> Min;
		Perturbed > Max -> Max;
		true -> Perturbed
	end.

dot([],[],Dot)->Dot;
dot([S|T],[W|K],Acc)->
	dot(T,K,Acc+S*W).


learn(GenoType,Output,Recv,[[]|_])->%è all'inizio,non ho segnali ricorsivi in entrata nel neurone!
	#neuron{faninsWeights=Ins}=GenoType,
	%io:fwrite("*****~nGENO: ~p~n INS: ~p~n RECV: ~p~n*****~n",[GenoType,Ins,Recv]),
	NewIns=plasticity:apply_plasticity(lists:keysort(1,Ins),lists:keysort(1,Recv),Output),
	GenoType#neuron{faninsWeights=NewIns};
learn(GenoType,Output,Recv,RoRecv)->%%inizia ad avere segnali ricorsivi in entrata
	#neuron{faninsWeights=Ins,roinsWeights=RoIns}=GenoType,
	%io:fwrite("****~nGENO: ~p~n INS: ~p~n RECV: ~p~n ROINS: ~p~n RORECV: ~p~n****~n",[GenoType,Ins,Recv,RoIns,RoRecv]),
	NewIns=plasticity:apply_plasticity(lists:keysort(1,Ins),lists:keysort(1,Recv),Output),
	NewRoIns=plasticity:apply_plasticity(lists:keysort(1,RoIns),lists:keysort(1,RoRecv),Output),
	GenoType#neuron{faninsWeights=NewIns,roinsWeights=NewRoIns}.