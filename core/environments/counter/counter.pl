

:-module(simple_counter,
    [
        simple_counter / 2,
	simple_counter / 3,			
	simple_counter / 4			
    ]
).


:-use_module('../FRAgPLEnvironmentUtils').   % interface for environments



% environment(simple_counter).


  
%    Situate agent in environment or its clone


init_beliefs(Agents):-
    query_environment(simple_counter, Agent, counter(Counter)),
    add_beliefs_agents(Agents, [counter(Counter)]).


simple_counter(add_agent, Agent):-
    situate_agent_environment(Agent, simple_counter),
    init_beliefs([Agent]).

simple_counter(add_agent, Agent, Clone):-
    situate_agents_clone([Agent], simple_counter, Clone),
    init_beliefs([Agent]).

simple_counter(add_agent, _, _).
 
%    Agent percieves

simple_counter(perceive, Agent , Add_List, Delete_List):-
     retreive_add_delete(Agent, Add_List, Delete_List).
      
%    Agent acts
                                
simple_counter(act, Agent, increase, true):- 
    query_environment(simple_counter, Agent, counter(Counter)),
    % _all -> pro vsechny agenty v tom samem env/clonu
    delete_facts_beliefs_all(simple_counter, Agent, [counter(Counter)]),
    Counter2 is Counter +1,
    add_facts_beliefs_all(simple_counter, Agent, [counter(Counter2)]).
  
simple_counter(act, Agent, decrease, true):-
    query_environment(simple_counter, Agent, counter(Counter)),
    delete_facts_beliefs_all(simple_counter, Agent, [counter(Counter)]),
    Counter2 is Counter -1,
    add_facts_beliefs_all(simple_counter, Agent, [counter(Counter2)]).

simple_counter(act, Agent, add(Number), true):-
    query_environment(simple_counter, Agent, counter(Counter)),
    delete_facts_beliefs_all(simple_counter, Agent, [counter(Counter)]),
    Counter2 is Counter +Number,
    add_facts_beliefs_all(simple_counter, Agent, [counter(Counter2)]).




simple_counter(act, Agent, silently_(increase), Result):-
    simple_counter(act, Agent, increase, Result).

simple_counter(act, Agent, silently_(decrease), Result):-
    simple_counter(act, Agent, decrease, Result).

simple_counter(act, Agent, silently_(add(Number)), Result):-
    simple_counter(act, Agent, add(Number), Result).




simple_counter(act, _, _, false).


simple_counter(clone, Clone):-
    clone_environment(simple_counter, Clone).
    

simple_counter(remove_clone, Clone):-
    remove_environment_clone(simple_counter, Clone).
 

simple_counter(reset_clone, Clone):-
    reset_environment_clone(simple_counter, Clone),
    get_all_situated(simple_counter, Clone, Agents),   
    init_beliefs(Agents).


simple_counter(save_state, Instance, State):-
    save_environment_instance_state(simple_counter, Instance, State).


simple_counter(load_state, Instance, State):-
    load_environment_instance_state(simple_counter, Instance, State).


simple_counter(remove_state, Instance, State):-
    remove_environment_instance_state(simple_counter, Instance, State).




:-
    env_utils:register_environment(simple_counter),
    env_utils:add_facts(simple_counter, [counter(1)]).
    

