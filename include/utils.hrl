%%MODULES REFERENCES
-define(ACTIVATION_FUNCTION_MODULE, math_utils).
-define(DATA_PROCESSING_MODULE, data_processing).
-define(NN_SERVICE_MODULE, nn_service).
-define(SOM_PHENOTYPE_MODULE, neuron_som).
-define(CLASSIC_PHENOTYPE_MODULE, neuron).
-define(CORTEX_MODULE, cortex).
-define(SENSOR_MODULE, sensor).
-define(ACTUATOR_MODULE, actuator).
%%

%%GENERAL UTILS
-define(GETID, utils:get_id()).
-define(RAND, rand:normal()).
-define(E, 2.71828182845904523536).
-define(MINPERTURB, (1 * math:pow(10, -308))).
-define(PROB(Sup), casuality_utils:prob_on_range(Sup)).
-define(SAT_LIMIT, (math:pi()*2)).
-define(RANDCHOOSE(List), casuality_utils:rand_choose(List)).
-define(NORMFIT(Fit), utils:normalize_fit(Fit)).
%%


