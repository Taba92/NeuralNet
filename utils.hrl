-define(GETID,list_to_atom(integer_to_list(logger:timestamp()+erlang:unique_integer([positive,monotonic])))).
-define(RAND,rand:uniform()-0.5).
-define(E,2.71828182845904523536).
-define(RANGE(Sup),round(math:floor(Sup))).
-define(PROB(Sup),1==rand:uniform(?RANGE(Sup))).
-define(SAT_LIMIT,?E).
-define(RANDCHOOSE(List),lists:nth(rand:uniform(length(List)),List)).

-record(population,{id,agents=[]}).
-record(agent,{id,scape,cortexId,genotype,fitness}).
-record(genotype,{sensors,neurons,actuators,cortex}).
-record(neuron,{id,layer,af,bias,faninsWeights,fanouts,roinsWeights,roouts}).
-record(sensor,{id,vl,fanouts}).
-record(cortex,{id,sensorsIds,neuronsIds,actuatorsIds}).
-record(actuator,{id,vl,fanins,cortexId}).

