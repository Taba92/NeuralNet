-module(plasticity).
-export([all/0,random_plasticity/1,get_rand_plast/2,get_plasticity/2,apply_plasticity/3]).
-define(PLAST(Weight),[?RAND||_<-lists:seq(1,length(Weight))]).
-include("utils.hrl").

get_rand_plast(Weight,PlastChoiches)->
	NewPlast=random_plasticity(PlastChoiches),
	get_plasticity(Weight,NewPlast).

random_plasticity(PlastChoiches)->
	NewPlast=?RANDCHOOSE(PlastChoiches),
	case NewPlast of
		neuromod->{neuromod,?RANDCHOOSE(lists:seq(0,5))};
		_->NewPlast
	end.

all()->
	[none,hebbian,oja,neuromod].

get_plasticity(_,none)->none;
get_plasticity(Weight,hebbian)->{hebbian,?PLAST(Weight)};
get_plasticity(Weight,oja)->{oja,?PLAST(Weight)};
get_plasticity(Weight,neuromod)->get_plasticity(Weight,{neuromod,0});
get_plasticity(Weight,{neuromod,N})when N=<5,N>=0->
	NeuronModulators=[{?PLAST(Weight),?RAND}||_<-lists:seq(1,N)],
	NormalModulators=[?PLAST(Weight)||_<-lists:seq(1,5-N)],
	{neuromod,N,NeuronModulators++NormalModulators}.

apply_plasticity(Weights,Signals,Output)->	
	apply_plasticity(Weights,Signals,Output,[]).

apply_plasticity([],_,_,Acc)->Acc;
apply_plasticity([{Id,Weight,none}|T],Signals,Output,Acc)->
	apply_plasticity(T,Signals,Output,Acc++[{Id,Weight,none}]);
apply_plasticity([{Id,Weight,{hebbian,LearnParams}}|T],Signals,Output,Acc)->
	{Id,Sig}=lists:keyfind(Id,1,Signals),
	NewInWeight={Id,hebbian(Weight,LearnParams,Sig,Output),{hebbian,LearnParams}},
	apply_plasticity(T,Signals,Output,Acc++[NewInWeight]);
apply_plasticity([{Id,Weight,{oja,LearnParams}}|T],Signals,Output,Acc)->
	{Id,Sig}=lists:keyfind(Id,1,Signals),
	NewInWeight={Id,oja(Weight,LearnParams,Sig,Output),{oja,LearnParams}},
	apply_plasticity(T,Signals,Output,Acc++[NewInWeight]);
apply_plasticity([{Id,Weight,{neuromod,N,LearnParams}}|T],Signals,Output,Acc)->
	{Id,Sig}=lists:keyfind(Id,1,Signals),
	NewInWeight={Id,neuromod(N,Weight,LearnParams,Sig,Output),{neuromod,N,LearnParams}},
	apply_plasticity(T,Signals,Output,Acc++[NewInWeight]).

hebbian(Weight,LearnParams,Sig,Output)->%W(t+1)=W(t)+H*InputSignal*Output
	A=dot(Sig,Output),
	B=dot(LearnParams,A),
	C=sum(Weight,B),
	[saturate(El,-?SAT_LIMIT,?SAT_LIMIT)||El<-C].

oja(Weight,LearnParams,Sig,Output)->%W(t+1)=W(t)+H*Output*(InputSignal–O*W(t))
	A=sub(Sig,dot(Weight,Output)),
	B=dot(Output,A),
	C=dot(LearnParams,B),
	D=sum(Weight,C),
	[saturate(El,-?SAT_LIMIT,?SAT_LIMIT)||El<-D].

neuromod(N,Weight,LearnParams,Sig,Output)->%W(t+1)=W(t)+H*(A*InputSignal*Output+B*InputSignal+C*Output+D),
	{Modulator,NotModulator}=lists:split(N,LearnParams),
	[H,A,B,C,D]=[modulate(Mod,Sig)||Mod<-Modulator]++NotModulator,
	P1=dot(A,dot(Sig,Output)),
	P2=dot(B,Sig),
	P3=dot(C,Output),
	P4=sum(P1,sum(P2,sum(P3,D))),
	P5=dot(H,P4),
	P6=sum(Weight,P5),
	[saturate(El,-?SAT_LIMIT,?SAT_LIMIT)||El<-P6].

modulate({Weight,Bias},Sig)->
	Dot=dot(Weight,Sig,0),
	[af:tanh(Dot+Bias)].

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