                        

% :-module(fRAgBlackboard,[

:-module(fa_sync,[ 
			frag_debug / 1,
			agent / 1,
			ready / 1,
%			create_and_lock_mutexes /0,
			sync_add_agent /1,
		   	sync_agents_ready /0,
			agent_salutes /1,
			b_step /1,
			running_agents/1
		]
	).


/** <module>  FRAg Environment Interface

Blackboard for distributed communication

@author Frantisek Zboril
@license GPL
*/


% Shared variables 

:-dynamic frag_debug / 1.
:-dynamic agent /1.				
:-dynamic ready /1.
:-dynamic go / 1.
:-dynamic max_agent_iterations /1.
:-dynamic mutexes /2.
:-dynamic add_agent /1.
:-dynamic agents_list /1.
:-dynamic running_agents /1.
:-dynamic b_step /1.
:-dynamic mutex /1.

agents_list([]).
running_agents([]).
b_step(0).



add_agent_list(Agent):-
   running_agents(L),
   retract(running_agents(L)),
   assert(running_agents([Agent|L])).

retract_agent_list(Agent):-
   running_agents(Agents),
   retract(running_agents(Agents)),
   delete(Agents, Agent, Agents2),
   assert(running_agents(Agents2)).


agent_salutes(Agent):-
  mutex(Mutex),
  (with_mutex(Mutex, (
   	running_agents(Running_Agents),
   	delete(Running_Agents, Agent, Running_Agents2),
%	writeln(Running_Agents),
%	writeln(Running_Agents2),
   	retract(running_agents( _ )),
   	assert(running_agents(Running_Agents2)),
   	step_finished(Running_Agents2)
   ))).



step_finished([]):-
   agents_list(Agents),
   retract(running_agents( _ )),
   assert(running_agents(Agents)),
   b_step(Step),
   Step2 is Step+1,
   retractall(b_step( _ )),
   assert(b_step(Step2)).


% some agents are still running previous loop step
step_finished( _ ).




            

sync_agents_ready:-
   agents_list(Agents),
   retract(running_agents( _ )),
   assert(running_agents(Agents)).   


 %  mutex_unlock(MutexA).
 %  mutex_init(Mutex_Init).
 %  mutex_unlock(Mutex_Init).

	

sync_add_agent(Agent):-
   agents_list(Agents),
   retract(agents_list( _ ) ),
   assert(agents_list([Agent| Agents])).
	

sync_init:-
   mutex_create(Mutex),
   assert(mutex(Mutex)),
   retract(agents_list( _ ) ),
   assert(agents_list([])),
   retract(b_step( _ )),
   assert(b_step(0)).



