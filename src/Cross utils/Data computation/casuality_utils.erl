-module(casuality_utils).
-export([rand_choose/1, prob_on_range/1]).

%This module provide functions inherent to stocastich events, like a launch of a dice ecc..

%%Given a List, return a random element of the List
rand_choose(List) when is_list(List) ->
	RandomIndex = rand:uniform(length(List)),
	lists:nth(RandomIndex, List).

%%Perform a launch of dice on a range of values from 1 to Sup
%%If Sup is minor than 1, it become 1
prob_on_range(Sup) when is_number(Sup) -> 
	FixedSup = case round(math:floor(Sup)) of 
					N when N < 1 -> 1;
					N -> N 
				end,
	1 == rand:uniform(FixedSup).
