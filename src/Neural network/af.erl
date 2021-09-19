-module(af).
-export([all_activation_functions_classic/0,tanh/1,threshold/1,identity/1,rectifier/1,sigmund/1,derivate/2]).
-export([all_activation_functions_som/0, euclidean/2]).
-include("utils.hrl").

%%%%%%%%%%%%%%%%%%%%%% FOR CLASSIC NEURONS%%%%%%%%%%
all_activation_functions_classic()->[tanh,rectifier,identity,threshold,sigmund].

threshold(X) when is_number(X) ->
	case X < 0 of 
		true -> 0;
		_ -> 1 
	end.

rectifier(X) when is_number(X) -> 
	max(0, X).
identity(X) when is_number(X) -> 
	X.
sigmund(X) when is_number(X) ->
	Function = fun() ->  1 / (1 + math:pow(?E, -X)) end,
	math_utils:catch_overflow(Function, 0).

tanh(X) when is_number(X) -> 
	(math:pow(?E, X) - math:pow(?E, -X)) / (math:pow(?E, X) + math:pow(?E, -X)).


%%It perform derivate function of given activation function name on input X
derivate(identity,_) -> 
	1;
derivate(sigmund, X) when is_number(X) -> 
	sigmund(X)*(1-sigmund(X));
derivate(tanh, X) when is_number(X) ->
	math:pow((1 / math:cosh(X)), 2);
derivate(rectifier, X) when is_number(X) -> 
	case X =< 0 of 
		true->0;
		false->1 
	end;
derivate(threshold, X) -> 
	case X=<0 of 
		true->0;
		false->1 
	end.
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%FOR SOM NEURONS%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all_activation_functions_som()->[euclidean].

euclidean(Vector1, Vector2 ) when is_list(Vector1), is_list(Vector2) -> 
	euclidean(Vector1, Vector2, 0).
euclidean([W | RestWeight], [S | RestSignal], Acc) when is_number(W), is_number(S), is_number(Acc)->
	EuclideanDistance = math:pow((W - S), 2),
	euclidean(RestWeight, RestSignal, Acc + EuclideanDistance);
euclidean([],[],Acc) when is_number(Acc) -> 
	math:sqrt(Acc).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%