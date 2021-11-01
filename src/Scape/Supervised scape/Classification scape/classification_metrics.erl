-module(classification_metrics).
-export([create_matrix_confusion/1,extract_info_label/2,incr_cell_matrix/3]).
-export([f1_score_avg/2,cross_entropy/2]).
-include("utils.hrl").


f1_score_avg(Classes,Matrix)->
	F1Scores=[f1_score(Class,Matrix)||Class<-Classes],
	lists:sum(F1Scores)/length(Classes).

f1_score(Class,Matrix)->
	Precision=precision(Class,Matrix),
	Recall=recall(Class,Matrix),
	2*((Precision*Recall)/(Precision+Recall+?MINPERTURB)).

precision(Class,Matrix)->
	#{tp:=Tp,fp:=Fp}=extract_info_label(Class,Matrix),
	Tp/(Tp+Fp+?MINPERTURB).

recall(Class,Matrix)->
	#{tp:=Tp,fn:=Fn}=extract_info_label(Class,Matrix),
	Tp/(Tp+Fn+?MINPERTURB).

cross_entropy(Target,Predict)->cross_entropy(Target,Predict,0).
cross_entropy([T|Tgs],[P|Preds],Acc)->
	Entropy=T*math:log(P+?MINPERTURB),
	cross_entropy(Tgs,Preds,Acc+Entropy);
cross_entropy([],[],Acc)->-Acc.

create_matrix_confusion(Labels)->[{Label,[{OtherLabel,0}||OtherLabel<-Labels]}||Label<-Labels].

extract_info_label(Label,Matrix)->
	#{tp=>tp_matrix(Label,Matrix),tn=>tn_matrix(Label,Matrix),fp=>fp_matrix(Label,Matrix),fn=>fn_matrix(Label,Matrix)}.

extract_cell_matrix(Lb1,Lb2,Matrix)->
	{Lb1,Row}=lists:keyfind(Lb1,1,Matrix),
	{Lb2,Val}=lists:keyfind(Lb2,1,Row),
	Val.

incr_cell_matrix(Lb1,Lb2,Matrix)->
	{Lb1,Row}=lists:keyfind(Lb1,1,Matrix),
	{Lb2,Val}=lists:keyfind(Lb2,1,Row),
	lists:keyreplace(Lb1,1,Matrix,{Lb1,lists:keyreplace(Lb2,1,Row,{Lb2,Val+1})}).

tp_matrix(Label,Matrix)->extract_cell_matrix(Label,Label,Matrix).

tn_matrix(Label,Matrix)->
	Labels=[Lb||{Lb,_}<-Matrix],
	lists:sum([extract_cell_matrix(Lb,Lb,Matrix)||Lb<-Labels--[Label]]).

fp_matrix(Label,Matrix)->
	Labels=[Lb||{Lb,_}<-Matrix],	
	lists:sum([extract_cell_matrix(Lb,Label,Matrix)||Lb<-Labels--[Label]]).

fn_matrix(Label,Matrix)->
	Labels=[Lb||{Lb,_}<-Matrix],
	lists:sum([extract_cell_matrix(Label,Lb,Matrix)||Lb<-Labels--[Label]]).