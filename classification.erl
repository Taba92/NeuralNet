-module(classification).
-export([start/2,start/3,extract_info/1,init/1,handle_call/3,is_finished/1,set_limit/2]).
-record(state,{type,readed,current,numRead,limit,funRead,dataset,info,fitAcc}).
-define(EXTRACT(Record),lists:split(length(Record)-1,Record)).
-define(READ(File),file:read_line(File)).
-include("utils.hrl").
-include_lib("kernel/include/file.hrl").

start(Dataset,Limit)->gen_server:start(?MODULE,[Dataset,Limit],[]).
start(Dataset,Fun,Limit)when is_function(Fun)->gen_server:start(?MODULE,[Dataset,Fun,Limit],[]).
extract_info(ScapeId)->gen_server:call(ScapeId,extract_info,infinity).
set_limit(ScapeId,Limit)->gen_server:call(ScapeId,{set_limit,Limit},infinity).

init([DatasetPath,Fun])when is_function(Fun)->
	{ok,Dataset}=file:open(DatasetPath,[read,raw,binary,{read_ahead,200000}]),
	State=#state{type=file,dataset=Dataset,numRead=0,funRead=Fun,fitAcc=0},
	{ok,State};
init([Dataset])->
	State=#state{type=list,readed=[],numRead=0,dataset=Dataset,fitAcc=0},
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
handle_call({set_limit,Limit},_,State)->
	{reply,ok,State#state{limit=round(Limit)}};
handle_call(reset,_,State)when State#state.type==list->
	#state{readed=Readed,current=Record,dataset=Dataset}=State,
	NewState=case Record of
				undefined->State#state{readed=[],current=undefined,dataset=Dataset++Readed,numRead=0,fitAcc=0};
				_->State#state{readed=[],current=undefined,dataset=Dataset++Readed++[Record],numRead=0,fitAcc=0}
			end,
	{reply,ok,NewState};
handle_call(reset,_,State) when State#state.type==file->
	#state{dataset=Dataset}=State,
	file:position(Dataset,bof),
	?READ(Dataset),
	NewState=State#state{readed=[],current=undefined,numRead=0,fitAcc=0},
	{reply,ok,NewState};
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
	#state{readed=Readed,current=Record,numRead=Num,limit=Limit,info=MapInfo,dataset=Dataset,fitAcc=FitAcc}=State,
	{_,Target}=?EXTRACT(Record),
	#{encoding:=Cod,len:=Len}=MapInfo,
	{Target,Encoding}=lists:keyfind(Target,1,Cod),
	PartialFit = ?NORMFIT((1-error_fun(Predict,Encoding))),
	case Dataset of
		[] ->
			Fitness =(FitAcc+PartialFit)/Len,
			NewState=State#state{readed=[],dataset=Dataset++Readed++[Record],numRead=0,fitAcc=0},
			Msg=#{type=>classification,partial_fit=>PartialFit,fitness=>Fitness,target=>Encoding,predict=>Predict},
			{reply,{finish,Msg},NewState};
		_ ->
			case Num==Limit of
				true->
					Fitness =(FitAcc+PartialFit)/Len,
					NewState=State#state{readed=Readed++[Record],numRead=0,fitAcc=0},
					Msg=#{type=>classification,partial_fit=>PartialFit,fitness=>Fitness,target=>Encoding,predict=>Predict},
					{reply,{finish,Msg},NewState};
				false->
					NewState=State#state{readed=Readed++[Record],numRead=Num+1,fitAcc=FitAcc+PartialFit},
					Msg=#{type=>classification,partial_fit=>PartialFit,target=>Encoding,predict=>Predict},
					{reply,{another,Msg},NewState}
			end
	end;
handle_call({action_fit,Predict},_,State)when State#state.type==file->
	#state{current=Record,info=MapInfo,numRead=Num,limit=Limit,dataset=Dataset,fitAcc=FitAcc}=State,
	{_,Target}=?EXTRACT(Record),
	#{encoding:=Cod,len:=Len}=MapInfo,
	{Target,Encoding}=lists:keyfind(Target,1,Cod),
	PartialFit = ?NORMFIT((1-error_fun(Predict,Encoding))),
	case is_finished(Dataset) of
		true->
			Fitness =(FitAcc+PartialFit)/Len,
			file:position(Dataset,bof),
			?READ(Dataset),
			NewState=State#state{numRead=0,fitAcc=0},
			Msg=#{type=>classification,partial_fit=>PartialFit,fitness=>Fitness,target=>Encoding,predict=>Predict},
			{reply,{finish,Msg},NewState};
		false ->
			case Num==Limit of
				true->
					Fitness =(FitAcc+PartialFit)/Len,
					NewState=State#state{numRead=0,fitAcc=0},
					Msg=#{type=>classification,partial_fit=>PartialFit,fitness=>Fitness,target=>Encoding,predict=>Predict},
					{reply,{finish,Msg},NewState};
				false->
					NewState=State#state{numRead=Num+1,fitAcc=FitAcc+PartialFit},
					Msg=#{type=>classification,partial_fit=>PartialFit,target=>Encoding,predict=>Predict},
					{reply,{another,Msg},NewState}
			end
	end;
handle_call({action_fit_predict,Predict},_,State)when State#state.type==list->
	#state{readed=Readed,current=Record,dataset=Dataset}=State,
	{_,Target}=?EXTRACT(Record),
	Msg=#{type=>classification,target=>Target,predict=>Predict},
	case Dataset of
		[] ->
			NewState=State#state{readed=[],dataset=Dataset++Readed++[Record]},
			{reply,{finish,Msg},NewState};
		_ ->
			NewState=State#state{readed=Readed++[Record]},
			{reply,{another,Msg},NewState}
	end;
handle_call({action_fit_predict,Predict},_,State)when State#state.type==file->
	#state{current=Record,dataset=Dataset}=State,
	{_,Target}=?EXTRACT(Record),
	Msg=#{type=>classification,target=>Target,predict=>Predict},
	case is_finished(Dataset) of
		true->
			file:position(Dataset,bof),
			?READ(Dataset),
			{reply,{finish,Msg},State};
		false ->
			{reply,{another,Msg},State}
	end;
handle_call({action_predict,_},_,State)->{reply,ok,State}.


error_fun(V1,V2)->manhattan(V1,V2).
manhattan(V1,V2)->manhattan(V1,V2,0,length(V1)).
manhattan([X|List1],[Y|List2],ErrorAcc,Num)->
	PartialError=erlang:abs(X-Y),
	manhattan(List1,List2,ErrorAcc+PartialError,Num);
manhattan([],[],ErrorAcc,Num)->ErrorAcc/Num.

euclidean(V1,V2)->euclidean(V1,V2,0).
euclidean([X|List1],[Y|List2],ErrorAcc)->
	PartialError=math:pow(X-Y,2),
	euclidean(List1,List2,ErrorAcc+PartialError);
euclidean([],[],ErrorAcc)->math:sqrt(ErrorAcc).


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
	NumClasses=length(Targets),
	#{mins=>NewMins,maxs=>NewMaxs,len=>Len,num_features=>NumFeatures,num_classes=>NumClasses,avgs=>Avgs,stds=>Stds,encoding=>Encoding}.

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
	NumClasses=length(Targets),
	#{mins=>NewMins,maxs=>NewMaxs,len=>Len,num_features=>NumFeatures,num_classes=>NumClasses,avgs=>Avgs,stds=>Stds,encoding=>Encoding}.

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