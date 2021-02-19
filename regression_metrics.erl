-module(regression_metrics).
-export([smape/2]).
-include("utils.hrl").


smape([Target],[Predict])->erlang:abs(Predict-Target)/((erlang:abs(Target)+erlang:abs(Predict))).
