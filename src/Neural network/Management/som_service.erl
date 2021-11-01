-module(som_service).
-export([actuator_get_signals/1, gaussian_neighborhood/3, get_BMU/1, get_BMU_cluster/1]).
-include("utils.hrl").

%Service for SOM neural network
actuator_get_signals(TupleList) ->
	[Value || {_,_,[Value] } <- TupleList].

gaussian_neighborhood(X, Y, NeighBoorSize) -> 
	math:pow(?E, -(math_utils:euclidean(X, Y) / 2 * math:pow(NeighBoorSize, 2))).

get_BMU(TupleList) when is_list(TupleList) -> 
	erlang:hd(lists:keysort(3, TupleList)).

get_BMU_cluster(TupleList) when is_list(TupleList) ->
	BMU = erlang:hd(lists:keysort(3, TupleList)),
	{_,{_, Cluster},_} = BMU,
	Cluster.