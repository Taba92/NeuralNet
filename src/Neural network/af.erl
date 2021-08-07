-module(af).
-export([all/0,tanh/1,threshold/1,identity/1,rectifier/1,sigmund/1,derivate/2]).
-export([euclidean/2]).
-include("utils.hrl").

%%%%%%%%%%%%%%%%%%%%%% FOR "NORMAL" NEURONS%%%%%%%%%%
all()->[tanh,rectifier,identity,threshold,sigmund].

threshold(X)->case X<0 of true->0;_->1 end.
rectifier(X)->max(0,X).
identity(X)->X.
sigmund(X)->try 1/(1+math:pow(?E,-X)) of Val->Val catch _:_->0 end.
tanh(X)->(math:pow(?E,X)-math:pow(?E,-X))/(math:pow(?E,X)+math:pow(?E,-X)).


derivate(identity,_)->1;
derivate(sigmund,X)->sigmund(X)*(1-sigmund(X));
derivate(tanh,X)->math:pow((1/math:cosh(X)),2);
derivate(rectifier,X)->case X=<0 of true->0;false->1 end;
derivate(threshold,X)->case X=<0 of true->0;false->1 end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%FOR SOM NEURONS%%%%%%%%%%%%%%%%%%%%%%%%%%%%
euclidean(Weight,Signal)when is_tuple(Weight),is_tuple(Signal)->euclidean(tuple_to_list(Weight),tuple_to_list(Signal),0);
euclidean(Weight,Signal)->euclidean(Weight,Signal,0).
euclidean([W|RestWeight],[S|RestSignal],Acc)->
	Dist=math:pow((W-S),2),
	euclidean(RestWeight,RestSignal,Acc+Dist);
euclidean([],[],Acc)->math:sqrt(Acc).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%