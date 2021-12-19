-module(utils).
-export([get_id/0, get_random_list/1, normalize_fit/1]).
-include("utils.hrl").

normalize_fit(Fit) -> 
	case is_number(Fit) of 
		true -> max(0, Fit);
		false -> Fit 
	end.

%%Return a unique atom identifier.
get_id() -> 
	list_to_atom(integer_to_list(logger:timestamp() + erlang:unique_integer([positive,monotonic]))).

get_random_list(Length) ->
	[?RAND || _ <- lists:seq(1, Length)].

