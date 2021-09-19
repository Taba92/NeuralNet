-module(math_utils).
-include("utils.hrl").
-export([catch_overflow/2, saturate/3]).
-export([lists_elements_dot/2, lists_elements_sum/2, lists_elements_sub/2]).

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
