-module(classification).
-export([start/1,start/2,extract_info/1,init/1,handle_call/3,is_finished/1]).
-record(state,{type,readed,current,funRead,dataset,info,errAcc}).
-define(EXTRACT(Record),lists:split(length(Record)-1,Record)).
-define(READ(File),file:read_line(File)).
-include("utils.hrl").
-include_lib("kernel/include/file.hrl").

start(Dataset)->gen_server:start(?MODULE,[Dataset],[]).
start(Dataset,Fun)when is_function(Fun)->gen_server:start(?MODULE,[Dataset,Fun],[]).
extract_info(ScapeId)->gen_server:call(ScapeId,extract_info,infinity).

init([DatasetPath,Fun])when is_function(Fun)->
	{ok,Dataset}=file:open(DatasetPath,[read,raw,binary,{read_ahead,200000}]),
	State=#state{type=file,dataset=Dataset,funRead=Fun,errAcc=0},
	{ok,State};
init([Dataset])->
	State=#state{type=list,readed=[],dataset=Dataset,errAcc=0},
	{ok,State}.

handle_call(extract_info,_,State)when State#state.type==list->
	#state{dataset=Dataset}=State,
	MapInfo=extract(list,Dataset),
	{reply,MapInfo,State#state{info=MapInfo}};
handle_call(extract_info,_,State)when State#state.type==file->
	#state{funRead=Fun,dataset=Dataset}=State,
	MapInfo=extract(file,Fun,Dataset),
	file:position(Dataset,bof),
	?READ(Dataset),
	{reply,MapInfo,State#state{info=MapInfo}};
handle_call(sense,_,State)when State#state.type==list->
	#state{dataset=[Record|T]}=State,
	{Features,_}=?EXTRACT(Record),
	NewState=State#state{current=Record,dataset=T},
	{reply,Features,NewState};
handle_call(sense,_,State)when State#state.type==file->
	#state{funRead=Fun,dataset=Dataset}=State,
	{ok,<<Line/binary>>}=?READ(Dataset),
	Record=Fun(Line),
	{Features,_}=?EXTRACT(Record),
	NewState=State#state{current=Record},
	{reply,Features,NewState};
handle_call({action_fit,Predict},_,State)when State#state.type==list->
	#state{readed=Readed,current=Record,info=MapInfo,dataset=Dataset,errAcc=ErrAcc}=State,
	{_,Target}=?EXTRACT(Record),
	#{encoding:=Cod,len:=Len,num_features:=Num}=MapInfo,
	{Target,Encoding}=lists:keyfind(Target,1,Cod),
	Error = list_compare(Predict,Encoding,0,Num),
	case Dataset of
		[] ->
			MSE = ErrAcc+Error,
			Fitness = 1-MSE/Len,
			NewState=State#state{readed=[],dataset=Readed++[Record],errAcc=0},
			{reply,{finish,Fitness,Encoding},NewState};
		_ ->
			NewState=State#state{readed=Readed++[Record],errAcc=ErrAcc+Error},
			{reply,{another,null,Encoding},NewState}
	end;
handle_call({action_fit,Predict},_,State)when State#state.type==file->
	#state{current=Record,info=MapInfo,dataset=Dataset,errAcc=ErrAcc}=State,
	{_,Target}=?EXTRACT(Record),
	#{encoding:=Cod,len:=Len,num_features:=Num}=MapInfo,
	{Target,Encoding}=lists:keyfind(Target,1,Cod),
	Error = list_compare(Predict,Encoding,0,Num),
	case is_finished(Dataset) of
		true->
			MSE = ErrAcc+Error,
			Fitness = 1-MSE/Len,
			file:position(Dataset,bof),
			?READ(Dataset),
			NewState=State#state{errAcc=0},
			{reply,{finish,Fitness,Encoding},NewState};
		false ->
			NewState=State#state{errAcc=ErrAcc+Error},
			{reply,{another,null,Encoding},NewState}
	end;
handle_call({action_predict,_},_,State)->{reply,ok,State}.


list_compare([X|List1],[Y|List2],ErrorAcc,Num)->
	PartialError=erlang:abs(X-Y),
	list_compare(List1,List2,ErrorAcc+PartialError,Num);
list_compare([],[],ErrorAcc,Num)->erlang:abs(ErrorAcc)/Num.

%%FOR FILE 

is_finished(Dataset)->
	{ok,Pos}=file:position(Dataset,cur),
	{ok,#file_info{size=Size}}=file:read_file_info(Dataset),
	Size==Pos.

extract(file,Fun,Dataset)->
	{ok,<<Line/binary>>}=?READ(Dataset),
	NumFeatures=get_num_features_file(Fun(Line)),
	Mins=Maxs=lists:duplicate(NumFeatures,none),
	Sums=Scarti=lists:duplicate(NumFeatures,0),
	{NewMins,NewMaxs,NewSums,Targets,Len}=extract_file(?READ(Dataset),Dataset,Fun,Mins,Maxs,Sums,[],0),
	Avgs=[Sum/Len||Sum<-NewSums],
	file:position(Dataset,bof),
	?READ(Dataset),
	Stds=extract_file(?READ(Dataset),Dataset,Fun,Avgs,Scarti,Len),
	Encoding=preprocess:one_hot(Targets),
	#{mins=>NewMins,maxs=>NewMaxs,len=>Len,num_features=>NumFeatures,avgs=>Avgs,stds=>Stds,encoding=>Encoding}.

extract_file(eof,_,_,NewMins,NewMaxs,NewSums,NewTargets,NewLen)->{NewMins,NewMaxs,NewSums,NewTargets,NewLen};
extract_file({ok,<<Line/binary>>},Dataset,Fun,Mins,Maxs,Sums,Targets,Len)->
	{Features,Target}=?EXTRACT(Fun(Line)),
	NewMins=extract_min(Features,Mins),
	NewMaxs=extract_max(Features,Maxs),
	NewSums=extract_sum(Features,Sums),
	NewTargets=extract_target(Target,Targets),
	NewLen=Len+1,
	extract_file(?READ(Dataset),Dataset,Fun,NewMins,NewMaxs,NewSums,NewTargets,NewLen).

extract_file(eof,_,_,_,NewScarti,Len)->extract_stds(NewScarti,Len);
extract_file({ok,<<Line/binary>>},Dataset,Fun,Avgs,Scarti,Len)->
	{Features,_}=?EXTRACT(Fun(Line)),
	NewScarti=extract_scarti(Features,Avgs,Scarti),
	extract_file(?READ(Dataset),Dataset,Fun,Avgs,NewScarti,Len).

get_num_features_file(Record)->length(Record)-1.
%%%%%
%%FOR LIST
extract(list,Dataset)->
	NumFeatures=get_num_features_list(Dataset),
	Mins=Maxs=lists:duplicate(NumFeatures,none),
	Sums=Scarti=lists:duplicate(NumFeatures,0),
	{NewMins,NewMaxs,NewSums,Targets,Len}=extract_list(Dataset,Mins,Maxs,Sums,[],0),
	Avgs=[Sum/Len||Sum<-NewSums],
	Stds=extract_list(Dataset,Avgs,Scarti,Len),
	Encoding=preprocess:one_hot(Targets),
	#{mins=>NewMins,maxs=>NewMaxs,len=>Len,num_features=>NumFeatures,avgs=>Avgs,stds=>Stds,encoding=>Encoding}.

extract_list([],NewMins,NewMaxs,NewSums,NewTargets,NewLen)->{NewMins,NewMaxs,NewSums,NewTargets,NewLen};
extract_list([Record|Dataset],Mins,Maxs,Sums,Targets,Len)->
	{Features,Target}=?EXTRACT(Record),
	NewMins=extract_min(Features,Mins),
	NewMaxs=extract_max(Features,Maxs),
	NewSums=extract_sum(Features,Sums),
	NewTargets=extract_target(Target,Targets),
	NewLen=Len+1,
	extract_list(Dataset,NewMins,NewMaxs,NewSums,NewTargets,NewLen).

extract_list([],_,NewScarti,Len)->extract_stds(NewScarti,Len);
extract_list([Record|Dataset],Avgs,Scarti,Len)->
	{Features,_}=?EXTRACT(Record),
	NewScarti=extract_scarti(Features,Avgs,Scarti),
	extract_list(Dataset,Avgs,NewScarti,Len).

get_num_features_list([Record|_])->length(Record)-1.
%%%%

extract_min(Signal,Mins)->extract_min(Signal,Mins,[]).
extract_min([],[],NewMins)->NewMins;
extract_min([H|T],[ActualMin|Mins],Acc)->
	case ActualMin==none orelse H<ActualMin of
		true->extract_min(T,Mins,Acc++[H]);
		false->extract_min(T,Mins,Acc++[ActualMin])
	end.

extract_max(Signal,Mins)->extract_max(Signal,Mins,[]).
extract_max([],[],NewMaxs)->NewMaxs;
extract_max([H|T],[ActualMax|Maxs],Acc)->
	case ActualMax==none orelse H>ActualMax of
		true->extract_max(T,Maxs,Acc++[H]);
		false->extract_max(T,Maxs,Acc++[ActualMax])
	end.

extract_sum(Signal,Sums)->extract_sum(Signal,Sums,[]).
extract_sum([],[],NewSums)->NewSums;
extract_sum([H|T],[ActualSum|Sums],Acc)->extract_sum(T,Sums,Acc++[H+ActualSum]).

extract_target(Target,Targets)->
	case lists:member(Target,Targets) of
		true->Targets;
		false->Targets++[Target]
	end.

extract_scarti(Signal,Avgs,Scarti)->extract_scarti(Signal,Avgs,Scarti,[]).
extract_scarti([],[],[],NewScarti)->NewScarti;
extract_scarti([H|T],[Avg|Avgs],[Scarto|Scarti],Acc)->extract_scarti(T,Avgs,Scarti,Acc++[math:pow((H-Avg),2)+Scarto]).

extract_stds(Scarti,Len)->extract_stds(Scarti,Len,[]).
extract_stds([],_,Stds)->Stds;
extract_stds([Scarto|Scarti],Len,Acc)->extract_stds(Scarti,Len,Acc++[math:sqrt(Scarto/Len)]).