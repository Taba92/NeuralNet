-module(sensorPheno).
-export([init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-record(state,{scapeId,genotype}).
-include("utils.hrl").

init(GenoType)when is_record(GenoType,sensor)->
	#sensor{id=Id}=GenoType,
	gen_server:start_link({local,Id},?MODULE,[GenoType],[]);
init([GenoType])->
	State=#state{genotype=GenoType},
	{ok,State}.

terminate(normal,_)->ok.

handle_call(dump,_,State)->
	#state{genotype=GenoType}=State,
	{reply,GenoType,State};
handle_call({set_scape,Scape},_,State)->
	{reply,ok,State#state{scapeId=Scape}}.
	
handle_cast(sync_fit,State)->
	#state{scapeId=Scape,genotype=GenoType}=State,
	#sensor{id=Id,fanouts=Pids}=GenoType,
	Signal=gen_server:call(Scape,sense),
	[gen_server:cast(Pid,{sensor,0,Id,forward_fit,Signal})||Pid<-Pids],
	%[Pid ! {sensor,0,Id,forward_fit,Signal}||Pid<-Pids],
	{noreply,State};
handle_cast({sync_predict,Signal},State)->
	#state{genotype=GenoType}=State,
	#sensor{id=Id,fanouts=Pids}=GenoType,
	[gen_server:cast(Pid,{sensor,0,Id,forward_predict,Signal})||Pid<-Pids],
	%[Pid ! {sensor,0,Id,forward_predict,Signal}||Pid<-Pids],
	{noreply,State}.