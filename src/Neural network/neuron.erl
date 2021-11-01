-module(neuron).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{received,roreceived,oldBias,oldWeights,oldRoWeights,lastOutput,lastSignals,genotype}).
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
					NewBias=perturbate_bias(Bias,Sup),
					NewFanIns=[perturbate_weight(Weight,Sup)||Weight<-Ins],
					NewRoIns=[perturbate_weight(RoWeight,Sup)||RoWeight<-RoIns],
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
	%{PrunRecv,PrunRoRecv}={NewRecv,NewRoRecv},
	{PrunRecv,PrunRoRecv}={prunSignals(NewRecv,length(Ins)),prunSignals(NewRoRecv,length(RoIns))},%possono esserci segnali doppiati!
	NewState=case length(PrunRecv)==length(Ins) andalso length(PrunRoRecv)==length(RoIns) of
		        	true->
		        		Dot=aggregate(PrunRecv,Ins)+aggregate(PrunRoRecv,RoIns),
						OutPut = ?ACTIVATION_FUNCTION_MODULE:Af(Dot+Bias),
						NewGenotype=learn(GenoType,[OutPut],PrunRecv,PrunRoRecv),
						[gen_server:cast(Pid,{neuron,Layer,Id,FwdType,[OutPut]})||Pid<-Outs++RoOuts],
						State#state{lastOutput={Dot,OutPut},lastSignals={PrunRecv,PrunRoRecv},received=[],roreceived=[],genotype=NewGenotype};
					false->
						State#state{received=NewRecv,roreceived=NewRoRecv}
			end,
	{noreply,NewState}.

prunSignals(Signals,Len)->%nel caso il numero di segnali ricevuti sia maggiore di quelli permessi allora scarta quelli in eccesso
	case length(Signals) =< Len of
		true -> Signals;
		false -> {LenSignals,_} = lists:split(Len,Signals),
				  LenSignals
	end.

aggregate([nil|_],_)->0;%nel caso avessi connessioni ricorsive e fossi all'inizio del fit
aggregate(Signals, Weights)->
	aggregate(Signals, Weights, 0). 
aggregate([],_,Acc)->Acc;
aggregate([{Id,Signal}|T],Weight,Acc)->
	{Id,W,_}=lists:keyfind(Id,1,Weight),
	aggregate(T, Weight, Acc + dot(Signal,W,Acc)).
	

perturbate_weight({Id,Weight,Modulator},Sup)->
	case ?PROB(Sup) of
		true->{Id,perturbate_vals(Weight),Modulator};
		false->{Id,Weight,Modulator}
	end.

perturbate_vals([]) -> [];
perturbate_vals([H|T]) -> [?NN_SERVICE_MODULE:perturbate(H) | perturbate_vals(T)].

perturbate_bias(Val,Sup)->
	case ?PROB(Sup) of
		true->?NN_SERVICE_MODULE:perturbate(Val);
		false->Val
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