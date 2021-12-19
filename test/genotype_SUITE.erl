-module(genotype_SUITE).
-export([all/0]).
-export([create_som/1, create_ffnn/1, create_rnn/1, create_rnn2/1]).
-include_lib("common_test/include/ct.hrl").
-include("genotype.hrl").

all() -> [create_som, create_ffnn, create_rnn, create_rnn2].


create_som(_)->
    SensorSpec={3,[],[]},
	ActuatorSpec={[],[{som_service,get_BMU_cluster,[]}]},
	CortexSpec={[{fun(List)->hd(List) end,[]}],[{fun(List)->hd(List) end,[]}]},
    Genotype = genotype:create_NN({som,euclidean},SensorSpec,ActuatorSpec,CortexSpec,{3,4}),
	print_genotype(Genotype).

create_ffnn(_) -> 
	SensorSpec={3,[],[]},
	ActuatorSpec={1,[],[]},
	CortexSpec={[{fun(List)->hd(List) end,[]}],[{fun(List)->hd(List) end,[]}]},
    Genotype = genotype:create_NN({ffnn, sigmund, hebbian}, SensorSpec, ActuatorSpec, CortexSpec, [3, 4]),
	print_genotype(Genotype).

create_rnn(_) -> 
	SensorSpec={4,[],[]},
	ActuatorSpec={1, [],[]},
	CortexSpec={[{fun(List)->hd(List) end,[]}],[{fun(List)->hd(List) end,[]}]},
    Genotype = genotype:create_NN({rnn, identity, none}, SensorSpec, ActuatorSpec, CortexSpec, [2,2,2]),
	print_genotype(Genotype).

create_rnn2(_) -> 
	SensorSpec={4,[],[]},
	ActuatorSpec={1, [],[]},
	CortexSpec={[{fun(List)->hd(List) end,[]}],[{fun(List)->hd(List) end,[]}]},
    Genotype = genotype:create_NN({{rnn,1}, identity, none}, SensorSpec, ActuatorSpec, CortexSpec, [2,2,2]),
	print_genotype(Genotype).


print_genotype(Genotype) ->
	Vertices = digraph:vertices(Genotype#genotype.network),
	Network = get_node_and_connection(Genotype#genotype.network, Vertices, []),
	io:fwrite("Net: ~p~n",[Network]).

get_node_and_connection(_, [], Acc) -> Acc;
get_node_and_connection(Graph, [El | T ], Acc) ->
	{El, Label} = digraph:vertex(Graph, El),
	Entranti = [ get_edge_label(in, Graph, digraph:edge(Graph, Edge))||Edge <- digraph:in_edges(Graph, El)],
	Uscenti = [ get_edge_label(out, Graph, digraph:edge(Graph, Edge))||Edge <- digraph:out_edges(Graph, El)],
	NewAcc = [#{label => Label, entranti => Entranti, uscenti => Uscenti} | Acc ],
	get_node_and_connection(Graph, T, NewAcc).

get_edge_label(in, Graph, {_, V1, _,_}) ->
	digraph:vertex(Graph, V1);
get_edge_label(out, Graph, {_, _, V2, _}) ->
	digraph:vertex(Graph, V2).

