-module(neuronPheno).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-export([perturbate/3]).
-record(state,{received,roreceived,oldBias,oldWeights,oldRoWeights,histOut,histSig,genotype}).
-include("utils.hrl").

init(GenoType)when is_record(GenoType,neuron)->
	#neuron{id=Id}=GenoType,
	gen_server:start_link({local,Id},?MODULE,[GenoType],[]);
init([GenoType])->
	#neuron{roinsWeights=Ro}=GenoType,
	N=length(Ro),
	State=#state{received=[],roreceived=lists:duplicate(N,nil),genotype=GenoType},
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
handle_call({perturb_weights,Prob,StepW},_,State)->
	#state{genotype=GenoType}=State,
	#neuron{bias=Bias,faninsWeights=Ins,roinsWeights=RoIns}=GenoType,
	NewState=case ?PROB(Prob) of
				true->
					Sup=(length(Ins)+length(RoIns)+1)*StepW/100,
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
	%io:fwrite("SIGNAL IN ~p~n",[{ElType,IdFrom,FromLayer,Signal}]),
	#state{received=Recv,roreceived=RoRecv,genotype=GenoType}=State,
	#neuron{id=Id,layer=Layer,af=Af,bias=Bias,faninsWeights=Ins,fanouts=Outs,roinsWeights=RoIns,roouts=RoOuts}=GenoType,
	{NewRecv,NewRoRecv}=case FromLayer>=Layer of
								true->{Recv,RoRecv++[{IdFrom,Signal}]};
								false->{Recv++[{IdFrom,Signal}],RoRecv}
								end,
	{PrunRecv,PrunRoRecv}={prunSignals(NewRecv,length(Ins)),prunSignals(NewRoRecv,length(RoIns))},%possono esserci segnali doppiati!
	NewState=case length(PrunRecv)==length(Ins) andalso length(PrunRoRecv)==length(RoIns) of
		        	true->
		        		Dot=aggregate(PrunRecv,Ins)+aggregate(PrunRoRecv,RoIns),
						OutPut=af:Af(Dot+Bias),
						NewGenotype=learn(GenoType,[OutPut],PrunRecv,PrunRoRecv),
						[gen_server:cast(Pid,{neuron,Layer,Id,FwdType,[OutPut]})||Pid<-Outs++RoOuts],
						State#state{histOut={Dot,OutPut},histSig={PrunRecv,PrunRoRecv},received=[],roreceived=[],genotype=NewGenotype};
					false->
						State#state{received=NewRecv,roreceived=NewRoRecv}
			end,
	{noreply,NewState}.

prunSignals(Signals,Len)->%nel caso il numero di segnali ricevuti sia maggiore di quelli permessi allora scarta quelli in eccesso
	case length(Signals)=<Len of
		true->Signals;
		false->{LenSignals,_}=lists:split(Len,Signals),
				LenSignals
	end.

aggregate([nil|_],_)->0;%nel caso avessi connessioni ricorsive e fossi all'inizio del fit
aggregate(Signals,Weights)->
	aggregate(Signals,Weights,0). 
aggregate([],_,Acc)->Acc;
aggregate([{Id,Signal}|T],Weight,Acc)->
	{Id,W,_}=lists:keyfind(Id,1,Weight),
	aggregate(T,Weight,Acc+dot(Signal,W,Acc)).
	

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
	saturate(?RAND*?SAT_LIMIT+Val,Min,Max).

saturate(Val,Min,Max)->
	if
		Val < Min -> Min;
		Val > Max -> Max;
		true -> Val
	end.

dot(Sig,Weight,_)when length(Sig)/=length(Weight)->
	error(bad_signal_for_weight);
dot([],[],Dot)->Dot;
dot([S|T],[W|K],Acc)->
	try dot(T,K,Acc+S*W) of Val->Val catch _:_->?RANDCHOOSE([-?SAT_LIMIT,?SAT_LIMIT]) end.

learn(GenoType,Output,Recv,[nil|_])->%è all'inizio,non ho segnali ricorsivi in entrata nel neurone!
	#neuron{faninsWeights=Ins}=GenoType,
	%io:fwrite("*****~nGENO: ~p~n INS: ~p~n RECV: ~p~n*****~n",[GenoType,Ins,Recv]),
	NewIns=plasticity:apply_plasticity(Ins,Recv,Output),
	GenoType#neuron{faninsWeights=NewIns};
learn(GenoType,Output,Recv,RoRecv)->%%inizia ad avere segnali ricorsivi in entrata
	#neuron{faninsWeights=Ins,roinsWeights=RoIns}=GenoType,
	%io:fwrite("****~nGENO: ~p~n INS: ~p~n RECV: ~p~n ROINS: ~p~n RORECV: ~p~n****~n",[GenoType,Ins,Recv,RoIns,RoRecv]),
	NewIns=plasticity:apply_plasticity(Ins,Recv,Output),
	NewRoIns=plasticity:apply_plasticity(RoIns,RoRecv,Output),
	GenoType#neuron{faninsWeights=NewIns,roinsWeights=NewRoIns}.