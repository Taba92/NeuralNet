-module(unsupervised_metrics).
-export([topographic_error/2]).

topographic_error(BMU,NodesOutput)->
	{IdBMU,_,_}=BMU,
	OtherNodes=NodesOutput--[BMU],%cavo il bmu dai vari nodi output
	SecondBmu=utils:get_BMU(OtherNodes),%prendo il secondo BMU
	{IdSecondBmu,_,_}=SecondBmu,
	NeighBoorsFirstBMU=gen_server:call(IdBMU,get_neighbors,infinity),
	case lists:member(IdSecondBmu,NeighBoorsFirstBMU) of
		true->0;
		false->1
	end.