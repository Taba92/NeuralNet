-define(GETID,utils:get_id()).
-define(RAND,rand:normal()).
-define(E,2.71828182845904523536).
-define(MINPERTURB,(1*math:pow(10,-308))).
-define(PROB(Sup),utils:prob_on(Sup)).
-define(SAT_LIMIT,math:pi()*2).
-define(RANDCHOOSE(List),utils:randchoose(List)).
-define(NORMFIT(Fit),utils:normalize_fit(Fit)).
-define(EXTRACT(Record),lists:split(length(Record)-1,Record)).
-define(READ(File),file:read_line(File)).


-record(population,{id,agents=[]}).
-record(agent,{id,scape,cortexId,genotype,fitness}).
-record(genotype,{sensors,neurons,actuators,cortex}).
-record(neuron,{id,layer,af,bias,faninsWeights,fanouts,roinsWeights,roouts}).
-record(sensor,{id,vl,fit_directives,real_directives,fanouts}).
-record(cortex,{id,sensorsIds,neuronsIds,actuatorsIds}).
-record(actuator,{id,vl,fit_directives,real_directives,fanins,cortexId}).

