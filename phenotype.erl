-module(phenotype).
-export([geno_to_pheno/1,pheno_to_geno/1]).
-export([stop_phenotype/1,backup_weights/1,perturb_weights/1,restore_weights/1]).
-export([link_to_cortex/2,link_nn_to_scape/2]).
-include("utils.hrl").

link_to_cortex(Agent,CortexId)->
	#agent{id=Id}=Agent,
	gen_server:call(CortexId,{controller_id,Id}),
	Agent#agent{cortexId=CortexId}.

link_nn_to_scape(Genotype,Scape)->
	[gen_server:call(SensorId,{set_scape,Scape})||SensorId<-genotype:get_sensors_ids(Genotype)],
	[gen_server:call(ActId,{set_scape,Scape})||ActId<-genotype:get_actuators_ids(Genotype)],
	ok.

geno_to_pheno(GenoType)when is_record(GenoType,genotype)->
	#genotype{sensors=SensorsGeno,neurons=NetGeno,actuators=ActuatorsGeno,cortex=CortexGeno}=GenoType,
	[sensorPheno:init(Sensor)||Sensor<-SensorsGeno],
	[neuronPheno:init(Neuron)||Neuron<-NetGeno],
	%%[neuron_som:init(Neuron)||Neuron<-NetGeno],
	[actuatorPheno:init(Actuator)||Actuator<-ActuatorsGeno],
	cortexPheno:init(CortexGeno).

pheno_to_geno(CortexId)->
	CortexGeno=gen_server:call(CortexId,dump,infinity),
	#cortex{sensorsIds=SensorsIds,neuronsIds=NeuronsIds,actuatorsIds=ActuatorsIds}=CortexGeno,
	SensorsGeno=[gen_server:call(SensorId,dump,infinity)||SensorId<-SensorsIds],
	NeuronsGeno=[gen_server:call(NeuronId,dump,infinity)||NeuronId<-NeuronsIds],
	ActuatorsGeno=[gen_server:call(ActuatorId,dump,infinity)||ActuatorId<-ActuatorsIds],
	#genotype{sensors=SensorsGeno,neurons=lists:keysort(3,NeuronsGeno),actuators=ActuatorsGeno,cortex=CortexGeno}.

stop_phenotype(CortexId)->
	CortexGeno=gen_server:call(CortexId,dump,infinity),
	#cortex{sensorsIds=SensorsIds,neuronsIds=NeuronsIds,actuatorsIds=ActuatorsIds}=CortexGeno,
	[gen_server:stop(Pid,normal,infinity)||Pid<-[CortexId]++SensorsIds++NeuronsIds++ActuatorsIds],
	ok.

backup_weights(CortexId)->
	CortexGeno=gen_server:call(CortexId,dump,infinity),
	#cortex{neuronsIds=NeuronsIds}=CortexGeno,
	[gen_server:call(Pid,backup_weights,infinity)||Pid<-NeuronsIds],
	ok.

perturb_weights({CortexId,Prob,StepW})->
	CortexGeno=gen_server:call(CortexId,dump,infinity),
	#cortex{neuronsIds=NeuronsIds}=CortexGeno,
	[gen_server:call(Pid,{perturb_weights,Prob,StepW},infinity)||Pid<-NeuronsIds],
	ok.

restore_weights(CortexId)->
	CortexGeno=gen_server:call(CortexId,dump,infinity),
	#cortex{neuronsIds=NeuronsIds}=CortexGeno,
	[gen_server:call(Pid,restore_weights,infinity)||Pid<-NeuronsIds],
	ok.