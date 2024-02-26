
:-module(env_utils,
    [
        register_environment /1,
        register_clone /2,   % nutne??
        clone_environment /2,
	environment_registered /2,
        situate_agent_environment /2,
        situate_agents_clone /3,
	agent_situated_environment /3,
        get_all_situated /3,
        situated_number /3,
	reset_environment_clone /2,
	remove_environment_clone /2,
        query_environment /3,
        findall_environment /4,
        add_beliefs /2,
	add_beliefs_agents /2,
        add_beliefs_all /2,
        add_facts /2,
        add_facts_clone /3,
        add_facts_agent /3,
        add_facts_beliefs /3,	
        add_facts_beliefs_all /3,
	delete_beliefs /2,
        delete_beliefs_agents /2,
	delete_beliefs_all /2,
        delete_facts_agent /3,
        delete_facts_beliefs /3,
        delete_facts_beliefs_all /3,
        retreive_add_delete /3,  
	new_episode /1,
	save_environment_instance_state /3,
        load_environment_instance_state /3,
	remove_environment_instance_state /3
    ]
).

/** <module>  Environments Supporting Clauses 

Work with instances, facts, beliefs, agents
 

@author Frantisek Zboril
@license GPL
*/




:-dynamic fact/3.
:-dynamic fact/4.
:-dynamic episode_list /1.

:-dynamic add/2.
:-dynamic delete/2.
:-dynamic environment /2.    % Registered environment / clone
:-dynamic situated_agent /3.  % Agents in this environment or clone


episode_list([]).



%!  register_environment(Environment) is det
%   Registers Environment or Clone of the Environment. System can tnen use 
%   Environment and or Clone of Environment, if these two names differs. 
%  @arg Environment: environment name

register_environment(Environment):-
    environment(Environment, _). % already registered

register_environment(Environment):-
    assert(environment(Environment, Environment)).


% not exported

register_clone(Environment, Clone):-
    environment(Environment, Clone). % already registered

register_clone(Environment, Clone):-
    assert(environment(Environment, Clone)).



%!  clone_environment(Environment, Clone) is det
%   Makes clone of Environment, for every fact(Environment, Environment, Fact),
%   creates clone fact(Environment, Clone, Fact),
%  @arg Environment: environment name
%  @arg Clone: clone name

clone_environment(Environment, Clone):-
    register_clone(Environment, Clone),
    findall(Fact, fact(Environment, Environment, Fact), Facts),
    add_facts_clone(Environment, Clone, Facts).



%!  remove_environment_clone(Environment, Clone)
%Removes all the facts of the clone and registration of agens to the clone
%* Environment: environment name
%* Clone: clone name

remove_environment_clone(Environment, Clone):-
    retractall(fact(Environment, Clone, _)),
    retractall(situated_agent(_, Environment, Clone)).



%!  environment_registered(Environment, Clone) is det
%   Retreives registered Environment and possible Clone
%  @arg Environment: environment name
%  @arg Clone: clone name

environment_registered(Environment, Clone):-
    environment(Environment, Clone).
       

 
%!  situate_agents_clone(+Agents, +Environment, +Clone) is det
%   Situates Agent in Environment, if Clone differs from Environment, then
%   the agent is virtual and interacts with Environment's Clone
%  @arg Agent: list of agent names / identifier 
%  @arg Environment: name of environment
%  @arg Clone: a possible clone of Environment

situate_agents_clone([], _, _).                    
                  
situate_agents_clone([Agent| Agents], Environment, Clone):-
    situated_agent(Agent, Environment, _),
    situate_agents_clone(Agents, Environment, Clone).

situate_agents_clone([Agent| Agents], Environment, Clone):-
    assert(situated_agent(Agent, Environment, Clone)),
    situate_agents_clone(Agents, Environment, Clone).



%!  situate_agent_environment(+Agent, +Environment) is det
%   Situates Agent in Environment original instance 
%  @arg Agent: agent name / identifier 
%  @arg Environment: name of environment

situate_agent_environment(Agent, Environment):-
    assert(situated_agent(Agent, Environment, Environment)).



%!  agent_situated_environment(+Agent, +Environment, +Clone) is det
%   Checkas whether Agent is situated in Environment or Environment's Clone if
%   Clone name differs from Environment name
%  @arg Agent: agent name / identifier 
%  @arg Environment: name of environment
%  @arg Clone: a possible clone of Environment

agent_situated_environment(Agent, Environment, Clone):-
   situated_agent(Agent, Environment, Clone).


get_clone_query(Environment, Agent, Query, fact(Environment, Clone, Query)):-
    situated_agent(Agent, Environment, Clone).     



%!  get_all_situated(+Environment, +Instance, -Agents) is det
%   Finds all Agents situated in Instance of Environment
%  @arg Environment: name of environment
%  @arg Instance: instance of Environment
%  @arg Agents: list of agents

get_all_situated(Environment, Instance, Agents):-
    findall(Agent, situated_agent(Agent, Environment, Instance), Agents).



%!  situated_number(+Environment, +Instance, - Number) is det
%   Provides number of Agents situated in Instance of Environment
%  @arg Environment: name of environment
%  @arg Instance: instance of Environment
%  @arg Number: number of agents situated in instance of name of environment

situated_number(Environment, Instance, Number):-
    get_all_situated(Environment, Instance, Agents),
    length(Agents, Number).



%! reset_environment_clone(+Environment, +Clone) is det
%Resets the clone, actualize it to the actual state of the original instance of 
%the environment
%* Environment: name of environment
%* Clone: some clone of Environment

reset_environment_clone(Environment, Clone):-
    get_all_situated(Environment, Clone, Agents),
    remove_environment_clone(Environment, Clone),
    clone_environment(Environment, Clone),
    situate_agents_clone(Agents, Environment, Clone).
    



findall_facts(Environment, Query, Facts):-
    findall(Query, fact(Environment, Environment, Query), Facts).

findall_facts(_, _, []).



%!  query_environment(+Environment, +Agent, +Query) is det
%   Queries given Environment or its clone where Agent is situated 
%  @arg Environment: which Environment 
%  @arg Agent: agent name / identifier 
%  @arg Query: query to Environment or its clone where Agent is situated

query_environment(Environment, Agent, Query):-
    get_clone_query(Environment, Agent, Query, Clone_Query),
    Clone_Query.

query_environment(Environment, Agent, Query):-
    situated_agent(Agent, Environment, Environment),
    fact(Environment, Environment, Query).



%!  findall_environment(+Environment, +Agent, +Query, -Answers) is det
%   Finds all the answers for Query given Environment or its clone where Agent is
%   situated (artefact? TODO)
%  @arg Environment: which Environment 
%  @arg Agent: agent name / identifier 
%  @arg Query: query to Environment or its clone where Agent is situated
%  @arg Answers: answers to Query from Environment / Clone where the Agent is

findall_environment(Environment, Agent, Query, Answers):-
    get_clone_query(Environment, Agent, Query, fact(Environment, Clone, 
                    Query)),
    findall(Query, fact(Environment, Clone, Query), Answers).


%
%	ADD and DELETE lists for agents
% 


%  Prepare Facts for agent's Add list
%  If there is equal Fact in Agent's Delete list -> removes it
%  Else it add the belief to the Agent's Add list.
%  add_belief(+Agent, +Facts)

add_belief(Agent, Belief):-
    delete(Agent, Belief),    % in Agent's delete list
    retract(delete(Agent, Belief)).

add_belief(Agent, Belief):-
    assert(add(Agent, Belief)).

%  Prepare Facts for agent's Delete list
%  If there is equal Fact in Agent's Add list -> remove it
%  Else it add the belief to the Agent's Delete list.
%  add_belief(+Agent, +Facts)

delete_belief(Agent, Belief):-
    add(Agent, Belief),
    retract(add(Agent, Belief)).

delete_belief(Agent, Belief):-
    assert(delete(Agent, Belief)).



%!  add_beliefs(+Agent, +Beliefs) is det
%   Inserts Beliefs to Agent's add list
%  @arg Agent: agent name / identifier 
%  @arg Beliefs: list of atoms or formulas

add_beliefs(_, []).

add_beliefs(Agent, [Belief| Beliefs]):-
    add_belief(Agent, Belief),
    add_beliefs(Agent, Beliefs).



%!  add_beliefs_agents(+Agents, +Beliefs) is det
%   Inserts Beliefs to add lists of the Agents 
%   environment (or clone of the environment)
%  @arg Agents: list of agents names / identifiers 
%  @arg Beliefs: list of atoms or formulas

add_beliefs_agents([], _).

add_beliefs_agents([Agent| Agents], Beliefs):-
    add_beliefs(Agent, Beliefs),
    add_beliefs_agents(Agents, Beliefs).



%!  add_beliefs_all(+Agent, +Beliefs) is det
%   Adds Beliefs to add lists of the Agent and all agents sharing the same
%   environment (or clone of the environment)
%  @arg Agent: agent name / identifier 
%  @arg Beliefs: list of atoms or formulas


add_beliefs_all(Agent, Beliefs):-
    situated_agent(Agent, Environment, Clone),
    get_all_situated(Environment, Clone, Agents),
    add_beliefs_agents(Agents, Beliefs).



%!  add_facts_agent(+Environment, +Agent, +Facts) is det
%   Asserts fact for Environment or its clone in which Agent is situated
%  @arg Environment: environment name
%  @arg Agent: agent name / identifier 
%  @arg Facts: list of atoms or formulas

add_facts_agent(_, _, []).

add_facts_agent(Environment, Agent, [Fact| Facts]):-
    get_clone_query(Environment, Agent, Fact, Clone_Fact),
    add_fact(Clone_Fact),
    add_facts_agent(Environment, Agent, Facts).



%!  add_facts(+Environment, +Facts) is det
%   Asserts facts for Environment's original instantion
%  @arg Environment: environment name
%  @arg Facts: list of atoms or formulas

add_facts( _, []).

add_facts(Environment, [Fact| Facts]):-
    add_fact(fact(Environment, Environment, Fact)),
    add_facts(Environment, Facts).


add_fact(Fact):-
    Fact.

add_fact(Fact):-
    assert(Fact).



%!  add_facts_clone(+Environment, +Instance, +Facts) is det
%   Asserts facts for Environment's Instance. If Instance is the same as 
%   Environment, then it is the main / original Environment instance
%  @arg Environment: environment name
%  @arg Instance: instance name 
%  @arg Facts: list of atoms or formulas

add_facts_clone( _, _, []).

add_facts_clone(Environment, Instance, [Fact| Facts]):-
    add_fact(fact(Environment, Instance, Fact)),
    add_facts_clone(Environment, Instance, Facts).



%!  add_facts_beliefs(+Environment, +Agent, +Beliefs) is det
%Add Beliefs to the Environment or its clone where Agent is
%and also process agent's add/delete lists. If belief to be added is in delete
%list, it is removed from it, else it is added to agent's add list
%* Environment: environment name
%* Agent: agent name
%* Beliefs: list of atoms


add_facts_beliefs(_, _, []).

add_facts_beliefs(Environment, Agent, Beliefs):-
    add_facts_agent(Environment, Agent, Beliefs),
    add_beliefs(Agent, Beliefs).

%!  add_facts_beliefs_all(+Environment, +Agent, +Beliefs) is det
%Add Beliefs to the Environment or its clone where Agent is
%and also process add/delete lists for all the agents in  Environent instance
%If belief to be added is in delete list, it is removed from it, else it is 
%added to agent's add list
%  @arg Environment: environment name
%  @arg Agent: agent name
%  @arg Beliefs: list of atoms


add_facts_beliefs_all(_, _, []).

add_facts_beliefs_all(Environment, Agent, Beliefs):-
    add_facts_agent(Environment, Agent, Beliefs),
    add_beliefs_all(Agent, Beliefs).




%!  delete_beliefs(+Agent, +Beliefs) is det
%   Removes Beliefs from Agent's add list
%  @arg Agent: agent name / identifier 
%  @arg Belief: list of atoms or formulas


delete_beliefs(_, []).

delete_beliefs(Agent, [Belief| Beliefs]):-
    delete_belief(Agent, Belief),
    delete_beliefs(Agent, Beliefs).



%!  delete_beliefs_agents(+Agents, +Beliefs) is det
%   Deletes Beliefs to add lists of the Agents environment or instance of the 
%   environment
%  @arg Agents: list of agents names / identifiers 
%  @arg Beliefs: list of atoms or formulas

delete_beliefs_agents([], _).

delete_beliefs_agents([Agent| Agents], Beliefs):-
    delete_beliefs(Agent, Beliefs),
    delete_beliefs_agents(Agents, Beliefs).



%!  delete_beliefs_all(+Agent, +Beliefs) is det
%   Delete Beliefs from delte lists of the Agent and all agents sharing the same 
%   environment (or clone of the environment)
%  @arg Agent: agent name / identifier 
%  @arg Beliefs: list of atoms or formulas


delete_beliefs_all(Agent, Beliefs):-
    situated_agent(Agent, Environment, Clone),
    get_all_situated(Environment, Clone, Agents),
    delete_beliefs_agents(Agents, Beliefs).


%!  delete_facts_agent(+Environment, +Agent, +Facts) is det
%   Retract facts from Environment or its clone in which Agent is situated
%  @arg Environment: environment name
%  @arg Agent: agent name / identifier 
%  @arg Facts: list of atoms or formulas


delete_facts_agent(_, _, []).

delete_facts_agent(Environment, Agent, [Fact| Facts]):-
    get_clone_query(Environment, Agent, Fact, Clone_Fact),
    try_retract(Clone_Fact),
    delete_facts_agent(Environment, Agent, Facts).

delete_facts_agent(Environment, Agent, [Belief| Beliefs]):-
    try_retract(fact(Environment, Environment, Belief)),
    delete_facts_agent(Environment, Agent, Beliefs).


try_retract(Clone_Fact):-
    retract(Clone_Fact).

try_retract(Clone_Fact):-
    format("[ERROR] retracting ~w failed~n", [Clone_Fact]).



%!  delete_facts_beliefs(+Environment, +Agent, +Beliefs) is det
%   Delete the beliefs to the Environment or its clone where Agent is
%   and also process agent's add/delete lists. If belief to be deleted is in add
%   list, it is removed from it, else it is added to agent's delete list
%  @arg Environment: environment name
%  @arg Agent: agent name
%  @arg Beliefs: list of atoms

delete_facts_beliefs(_, _, []).

delete_facts_beliefs(Environment, Agent, Facts):-
    delete_facts_agent(Environment, Agent, Facts),
    delete_beliefs(Agent, Facts).  



%!  delete_facts_beliefs_all(Environment, Agent, Beliefs) is det
%   Deletes Beliefs from the Environment or its clone where Agent is
%   and also process add/del lists for all the agents in an Environent instance
%   If belief to be deleted is in add list, it is removed from it, else it is 
%   added to agent's delete list
%  @arg Environment: environment name
%  @arg Agent: agent name
%  @arg Beliefs: list of atoms


delete_facts_beliefs_all(_, _, []).

delete_facts_beliefs_all(Environment, Agent, Beliefs):-
    delete_facts_agent(Environment, Agent, Beliefs),
    delete_beliefs_all(Agent, Beliefs).



%!  retreive_add_delete(+Agent, -Add_List, -Delete_List) is det
%   Provides add/delete lists for Agent 
%  @arg Agent: agent name
%  @arg Add_List: new percepts
%  @arg Delete_List: lost percepts

retreive_add_delete(Agent, Add_List, Delete_List):-
    get_add(Agent, Add_List),
    get_delete(Agent, Delete_List).


get_add(Agent, Add_List):-
    findall(Belief, add(Agent, Belief), Add_List),
    retractall(add(Agent, _)).

get_delete(Agent, Delete_List):-
    findall(Belief, delete(Agent, Belief), Delete_List),
    retractall(delete(Agent, _)).



%!  new_episode(+Agent) is det
%   Succeeds when Agent signalizes for the second time in episode. It adds 
%   Agent to a list of agents that wisited an environment and when is some 
%   to be added already in that list new episode begins. The new list contains
%   only the actual agent
%  @arg Agent: name of siglalizing agent

new_episode(Agent):-
    episode_list(Agents),
    member(Agent, Agents),
    retract(episode_list( _ )),
    assert(episode_list([Agent])).

new_episode(Agent):-
    episode_list(Agents),
    retract(episode_list( _ )),
    assert(episode_list([Agent | Agents])),
    !,
    fail.



%!  save_environment_instance_state(+Environment, +Instance, +State) is det
%   Saves actual state of Instance of Environment with name State
%   If State already exists, it is rewriten
%  @arg Environment 
%  @arg Instance
%  @arg State

save_environment_instance_state(Environment, Instance, State):-
    findall(Fact, fact(Environment, Instance, Fact), Facts),
    add_facts_state(Environment, Instance, State, Facts).


add_facts_state( _, _, _, []).

add_facts_state(Environment, Instance, State, [Fact| Facts]):-
    add_fact(fact(Environment, Instance, State, Fact)),
    add_facts_state(Environment, Instance, State, Facts).


 
%!  load_environment_instance_state(+Environment, +Instance, +State) is semidet
%   Loads state of Instance of Environment and revivews beliefs for all the agents
%   situated in the Instance. Fails when there does not exist saved State for the
%   Environment Instance 
%  @arg Environment 
%  @arg Instance
%  @arg State

load_environment_instance_state(Environment, Instance, State):-
    % get any agent in the instance
    agent_situated_environment(Agent, Environment, Instance),
    % clears Instance of Environment
    findall(Fact, fact(Environment, Instance, Fact), Facts1),
    delete_facts_beliefs_all(Environment, Agent, Facts1),
    !,
    findall(Fact, 
            fact(Environment, Instance, State, Fact), 
            Facts2),
    add_facts_beliefs_all(Environment, Agent, Facts2).



%!  remove_environment_instance_state(+Environment, +Instance, +State) is det
%   Removes state of Instance of Environment
%  @arg Environment 
%  @arg Instance
%  @arg State

remove_environment_instance_state(Environment, Instance, State):-
    retractall(fact(Environment, Instance, State, _)).

md:-
    use_module(library(pldoc/doc_library)),
 %   doc_load_library,
    doc_save('FRAgPLEnvironmentUtils.pl',[format(html), recursive(true), 
                                          doc_root('../../doc')]).







                                                  