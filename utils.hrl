-define(GETID,list_to_atom(integer_to_list(logger:timestamp()+erlang:unique_integer([positive,monotonic])))).
-define(RAND,(rand:uniform()-0.5)).
-define(E,2.71828182845904523536).
-define(RANGE(Sup),case round(math:floor(Sup)) of N when N<1->1;N->N end).
-define(PROB(Sup),1==rand:uniform(?RANGE(Sup))).
-define(SAT_LIMIT,math:pi()*2).
-define(RANDCHOOSE(List),lists:nth(rand:uniform(length(List)),List)).
-define(NORMFIT(Fit),case is_number(Fit) of true->max(0,Fit);false->Fit end).

-record(population,{id,agents=[]}).
-record(agent,{id,scape,cortexId,genotype,fitness}).
-record(genotype,{sensors,neurons,actuators,cortex}).
-record(neuron,{id,layer,af,bias,faninsWeights,fanouts,roinsWeights,roouts}).
-record(sensor,{id,vl,fit_directives,real_directives,fanouts}).
-record(cortex,{id,sensorsIds,neuronsIds,actuatorsIds}).
-record(actuator,{id,vl,fit_directives,real_directives,fanins,cortexId}).

