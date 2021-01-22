-module(af).
-export([all/0,tanh/1,threshold/1,rectifier/1,sigmund/1]).
-include("utils.hrl").

%all()->[tanh,rectifier,threshold,sigmund].
all()->[tanh,threshold,sigmund].

threshold(X)->case X<0 of true->0;_->1 end.
rectifier(X)->max(0,X).
sigmund(X)->1/(1+math:pow(?E,-1*X)).
tanh(X)->(math:pow(?E,X)-math:pow(?E,-1*X))/(math:pow(?E,X)+math:pow(?E,-1*X)).
