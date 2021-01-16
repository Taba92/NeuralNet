-module(af).
-export([get_afs/0,iperbolic/1,threshold/1,rectifier/1,sigmund/1]).
-include("utils.hrl").

get_afs()->[iperbolic,rectifier,threshold,sigmund].
%get_afs()->[iperbolic,threshold,sigmund].

threshold(X)->case X<0 of true->0;_->1 end.
rectifier(X)->max(0,X).
sigmund(X)->1/(1+math:pow(?E,-1*X)).
iperbolic(X)->(math:pow(?E,X)-math:pow(?E,-1*X))/(math:pow(?E,X)+math:pow(?E,-1*X)).
