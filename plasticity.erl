-module(plasticity).
-export([get_plasticity/2,apply_plasticity/3,rand_plast/2]).
-define(PLAST(Weight),[?RAND||_<-lists:seq(1,length(Weight))]).
-include("utils.hrl").

rand_plast(Weight,none)->
	NewPlast=?RANDCHOOSE([hebbian,oja,{neuromod,?RANDCHOOSE(lists:seq(0,5))}]),
	get_plasticity(Weight,NewPlast);
rand_plast(Weight,{hebbian,_})->
	NewPlast=?RANDCHOOSE([none,oja,{neuromod,?RANDCHOOSE(lists:seq(0,5))}]),
	get_plasticity(Weight,NewPlast);
rand_plast(Weight,{oja,_})->
	NewPlast=?RANDCHOOSE([hebbian,none,{neuromod,?RANDCHOOSE(lists:seq(0,5))}]),
	get_plasticity(Weight,NewPlast);
rand_plast(Weight,{neuromod,N,_})->
	NewPlast=?RANDCHOOSE([none,hebbian,oja,{neuromod,?RANDCHOOSE(lists:seq(0,5)--[N])}]),
	get_plasticity(Weight,NewPlast).

get_plasticity(_,none)->none;
get_plasticity(Weight,hebbian)->{hebbian,?PLAST(Weight)};
get_plasticity(Weight,oja)->{oja,?PLAST(Weight)};
get_plasticity(Weight,neuromod)->get_plasticity(Weight,{neuromod,0});
get_plasticity(Weight,{neuromod,N})when N=<5,N>=0->
	M={?PLAST(Weight),?RAND},
	NeuronModulators=[{?PLAST(Weight),?RAND}||_<-lists:seq(1,N)],
	NormalModulators=[?PLAST(Weight)||_<-lists:seq(1,5-N)],
	{neuromod,N,[M]++NeuronModulators++NormalModulators}.

apply_plasticity(Weights,Signals,Output)->	
	apply_plasticity(Weights,Signals,Output,[]).

apply_plasticity([],[],_,Acc)->Acc;
apply_plasticity([{Id,Weight,none}|T],[{Id,_}|K],Output,Acc)->
	apply_plasticity(T,K,Output,Acc++[{Id,Weight,none}]);
apply_plasticity([{Id,Weight,{hebbian,LearnParams}}|T],[{Id,Sig}|K],Output,Acc)->
	NewInWeight={Id,hebbian(Weight,LearnParams,Sig,Output),{hebbian,LearnParams}},
	apply_plasticity(T,K,Output,Acc++[NewInWeight]);
apply_plasticity([{Id,Weight,{oja,LearnParams}}|T],[{Id,Sig}|K],Output,Acc)->
	NewInWeight={Id,oja(Weight,LearnParams,Sig,Output),{oja,LearnParams}},
	apply_plasticity(T,K,Output,Acc++[NewInWeight]);
apply_plasticity([{Id,Weight,{neuromod,N,LearnParams}}|T],[{Id,Sig}|K],Output,Acc)->
	NewInWeight={Id,neuromod(N,Weight,LearnParams,Sig,Output),{neuromod,N,LearnParams}},
	apply_plasticity(T,K,Output,Acc++[NewInWeight]).


hebbian(Weight,LearnParams,Sig,Output)->
	A=dot(Sig,Output),
	B=dot(LearnParams,A),
	C=sum(Weight,B),
	[saturate(El,-?SAT_LIMIT,?SAT_LIMIT)||El<-C].

oja(Weight,LearnParams,Sig,Output)->
	A=sub(Sig,dot(Weight,Output)),
	B=dot(Output,A),
	C=dot(LearnParams,B),
	D=sum(Weight,C),
	[saturate(El,-?SAT_LIMIT,?SAT_LIMIT)||El<-D].

neuromod(N,Weight,LearnParams,Sig,Output)->
	[M|OtherLearnParams]=LearnParams,
	ModM=modulate(M,Sig),
	NewM=[scale_dzone(El,0.33,?SAT_LIMIT)||El<-ModM],
	{Modulator,NotModulator}=lists:split(N,OtherLearnParams),
	[H,A,B,C,D]=[modulate(Mod,Sig)||Mod<-Modulator]++NotModulator,
	P1=dot(A,dot(Sig,Output)),
	P2=dot(B,Sig),
	P3=dot(C,Output),
	P4=sum(P1,sum(P2,sum(P3,D))),
	P5=dot(dot(NewM,H),P4),
	P6=sum(Weight,P5),
	[saturate(El,-?SAT_LIMIT,?SAT_LIMIT)||El<-P6].

modulate({Weight,Bias},Sig)->
	Dot=dot(Weight,Sig,0),
	[af:iperbolic(Dot+Bias)].

scale_dzone(Val,Threshold,MaxMagnitude)->
	if
		Val > Threshold ->(scale(Val,MaxMagnitude,Threshold)+1)*MaxMagnitude/2;
		Val < -Threshold ->(scale(Val,-Threshold,-MaxMagnitude)-1)*MaxMagnitude/2;
		true ->0
	end.

scale(Val,Max,Min)->
	case Max == Min of
		true ->0;
		false ->(Val*2 - (Max+Min))/(Max-Min)
	end.

saturate(C,Min,Max)->
	if
		C < Min ->Min;
		C > Max -> Max;
		true -> C
	end.

dot(L1,L2)when length(L1)/=length(L2)->[X*Y||X<-L1,Y<-L2];
dot(L1,L2)->[X*Y||{X,Y}<-lists:zip(L1,L2)].

dot([],[],Dot)->Dot;
dot([S|T],[W|K],Acc)->
	dot(T,K,Acc+S*W).

sum(L1,L2)when length(L1)/=length(L2)->[X+Y||X<-L1,Y<-L2];
sum(L1,L2)->[X+Y||{X,Y}<-lists:zip(L1,L2)].

sub(L1,L2)when length(L1)/=length(L2)->[X-Y||X<-L1,Y<-L2];
sub(L1,L2)->[X-Y||{X,Y}<-lists:zip(L1,L2)].