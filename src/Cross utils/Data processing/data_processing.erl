-module(data_processing).
-export([one_hot/1,label/1,decode/2,encode/2,most_likely/1,softmax/1]).
-export([min_max_global/4,min_max_local/2,mean_global/4,mean_local/1,standardization_global/3,standardization_local/1]).

%This module provides several common utility functions to change raw feature vectors into a representation 
%that is more suitable for a neural network, tipical functions of encoding and scaling.
one_hot(Targets)when is_list(Targets)->
	one_hot(Targets,length(Targets),1,[]).
one_hot([],_,_,Acc)->Acc;
one_hot([H|T],Number,Pos,Acc)->
	case lists:member(H,T) of
		true->one_hot(T,Number,Pos,Acc);
		false->
			Encode=lists:duplicate(Pos-1,0)++[1]++lists:duplicate(Number-Pos,0),
			one_hot(T,Number,Pos+1,Acc++[{H,Encode}])
	end.	

label(Targets)when is_list(Targets)->
	label(lists:sort(Targets),length(Targets),0,[]).
label([],_,_,Acc)->Acc;
label([H|T],Len,Encode,Acc)->
	case lists:member(H,T) of
		true->label(T,Len,Encode,Acc);
		false->label(T,Len,Encode+1,Acc++[{H,Encode}])
	end.

decode(Value,Encoding)->
	case lists:keyfind(Value,2,Encoding) of
		{Label,Value}->Label;
		false->no_label_decoding
	end.

encode(Value,Encoding)->
	case lists:keyfind(Value,1,Encoding) of
		{Value,Encode}->Encode;
		false->no_label_encoding
	end.

%Given a vector of probabilities, return the most likely value
most_likely(Predict) ->
	MostProb=lists:max(Predict),
	A=fun(Prob,{Class,Find})->case (Prob == MostProb) and (Find /= true) of
								true->{Class++[1],true};
								false->{Class++[0],Find}
							end
		end,
	{Label,_}=lists:foldl(A,{[],false},Predict),
	Label.

softmax(Vector)when is_list(Vector)->
	Den = lists:sum([math:exp(X) || X <- Vector]),
	[math:exp(X)/Den||X<-Vector].

min_max_global(Record,Mins,Maxs,{A,B})when is_list(Record),is_list(Mins),is_list(Maxs),A<B->
	min_max(Record,Mins,Maxs,{A,B},[]).
min_max([],[],[],_,ScaledRd)->ScaledRd;
min_max([H|T],[Min|Mins],[Max|Maxs],{A,B},Acc)->
	Scaled=A+((H-Min)*(B-A))/(Max-Min),
	min_max(T,Mins,Maxs,{A,B},Acc++[Scaled]).

min_max_local(Record,{A,B})->
	Min=lists:min(Record),
	Max=lists:max(Record),
	Scale=fun(X)->A+((X-Min)*(B-A))/(Max-Min) end,
	[Scale(El)||El<-Record].

mean_global(Record,Mins,Maxs,Avgs)when is_list(Record),is_list(Mins),is_list(Maxs),is_list(Avgs)->
	mean(Record,Mins,Maxs,Avgs,[]).
mean([],[],[],[],ScaledRd)->ScaledRd;
mean([H|T],[Min|Mins],[Max|Maxs],[Avg|Avgs],Acc)->
	Scaled=(H-Avg)/(Max-Min),
	mean(T,Mins,Maxs,Avgs,Acc++[Scaled]).

mean_local(Record)->
	Min=lists:min(Record),
	Max=lists:max(Record),
	Avg=lists:sum(Record)/length(Record),
	Scale=fun(X)->(X-Avg)/(Max-Min) end,
	[Scale(El)||El<-Record].

standardization_global(Record,Avgs,StdDevs)->
	standardization(Record,Avgs,StdDevs,[]).
standardization([],[],[],ScaledRd)->ScaledRd;
standardization([H|T],[Avg|Avgs],[StdDev|StdDevs],Acc)->
	Scaled=(H-Avg)/StdDev,
	standardization(T,Avgs,StdDevs,Acc++[Scaled]).

standardization_local(Record)->
	Avg=lists:sum(Record)/length(Record),
	StdDev=math:sqrt(lists:sum([math:pow(El-Avg,2)||El<-Record])/length(Record)),
	Scale=fun(X)->(X-Avg)/StdDev end,
	[Scale(Element)||Element<-Record].
