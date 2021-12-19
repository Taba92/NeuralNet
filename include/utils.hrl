-define(GETID, utils:get_id()).
-define(RAND, rand:normal()).
-define(E, 2.71828182845904523536).
-define(ACTIVATION_FUNCTION_MODULE, math_utils).
-define(DATA_PROCESSING_MODULE, data_processing).
-define(NN_SERVICE_MODULE, nn_service).
-define(MINPERTURB, (1 * math:pow(10, -308))).
-define(PROB(Sup), casuality_utils:prob_on_range(Sup)).
-define(SAT_LIMIT, (math:pi()*2)).
-define(RANDCHOOSE(List), casuality_utils:rand_choose(List)).
-define(NORMFIT(Fit), utils:normalize_fit(Fit)).



