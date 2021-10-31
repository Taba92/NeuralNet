-module(math_utils).
-include("utils.hrl").
% General math utilities
-export([catch_overflow/2, saturate/3]).
% Activation functions for neurons
-export([all_activation_functions_classic/0,tanh/1,threshold/1,identity/1,rectifier/1,sigmund/1,derivate/2]).
-export([all_activation_functions_som/0, euclidean/2]).
% Vectors operations
-export([lists_elements_dot/2, lists_elements_sum/2, lists_elements_sub/2]).

% Section of general math utilities
%%Try to perform an Operation and in case of overflow error it return the OverFlowOutPut
catch_overflow(Operation, OverFlowOutPut) when is_function(Operation) ->
    try
        Operation()
    of
        Val -> Val
    catch
        _:_ -> OverFlowOutPut 
    end.

%Given a Value and a range Min<X<Max
%mantain the Value in the range if it exceed
saturate(Value,Min,Max)->
	if
		Value < Min -> Min;
		Value > Max -> Max;
		true -> Value
	end.

% Section of activation functions
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


%%It perform derivate function of given function name on input X
derivate(identity,_) -> 
	1;
derivate(sigmund, X) when is_number(X) -> 
	sigmund(X) * (1 - sigmund(X));
derivate(tanh, X) when is_number(X) ->
	math:pow((1 / math:cosh(X)), 2);
derivate(rectifier, X) when is_number(X) -> 
	case X =< 0 of 
		true -> 0;
		false -> 1 
	end;
derivate(threshold, X) -> 
	case X =< 0 of 
		true -> 0;
		false -> 1 
	end.
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%FOR SOM NEURONS%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all_activation_functions_som()->[euclidean].

euclidean(Vector1, Vector2) when is_list(Vector1), is_list(Vector2) -> 
	euclidean(Vector1, Vector2, 0).
euclidean([W | RestWeight], [S | RestSignal], Acc) when is_number(W), is_number(S), is_number(Acc)->
	EuclideanDistance = math:pow((W - S), 2),
	euclidean(RestWeight, RestSignal, Acc + EuclideanDistance);
euclidean([],[],Acc) when is_number(Acc) -> 
	math:sqrt(Acc).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%Section of vectors algebra

%%Given two list return a list of dot multiplication between them elements
%%If the lists have differents length it return a dot list from combination between each elements of the two list
%%else return a dot list from elements coupled in sequence of the two lists
lists_elements_dot(List1, List2) when length(List1) /= length(List2) -> 
    [catch_overflow(fun() -> X * Y end, ?SAT_LIMIT) || X <- List1, Y <- List2];
lists_elements_dot(List1, List2) when length(List1) == length(List2)-> 
    [catch_overflow(fun() -> X * Y end, ?SAT_LIMIT) || {X,Y} <- lists:zip(List1, List2)].

%%Given two list return a list of sum between them elements
%%If the lists have differents length it return a sum list from permutation between each elements of the two list
%%else return a list from sum elements coupled in sequence of the two lists
lists_elements_sum(List1, List2) when length(List1) /= length(List2) ->
    [X + Y || X <- List1, Y <- List2];
lists_elements_sum(List1, List2) when length(List1) == length(List2) ->
    [X + Y || {X,Y} <- lists:zip(List1, List2)].

%%Given two list return a list of substraction between them elements
%%If the lists have differents length it return a substraction list from combination between each elements of the two list
%%else return a list from substraction elements coupled in sequence of the two lists
lists_elements_sub(List1, List2) when length(List1) /= length(List2) ->
    [X - Y || X <- List1, Y <- List2];
lists_elements_sub(List1, List2) when length(List1) == length(List2) ->
    [X - Y || {X, Y} <- lists:zip(List1, List2)].
