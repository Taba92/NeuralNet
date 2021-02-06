-module(af).
-export([all/0,tanh/1,threshold/1,identity/1,rectifier/1,sigmund/1,derivate/2]).
-include("utils.hrl").

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
