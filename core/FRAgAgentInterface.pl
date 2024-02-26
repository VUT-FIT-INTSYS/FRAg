
% TODO Instances or Clones?

:-module(fRAgAgentInterface,
	    [
		add_environment_library /1,             % + init ihned
                set_environment_attributes /2,
	        environment_loaded /1,
		clone_environment /2,
		situate_agent /2,		% + agent, environment
		situate_agent /3,		% + agent, environment, clone
		get_agent_environments /2,
		get_all_environments /1,
                get_agent_instance /3,	% evn + clone
		agent_perceives /3,
		agent_acts /4,
		virtualize_agent /2,
		virtualize_agents /2,
		reset_clone /2,
		reset_clones /1,
		remove_clone /2,
		remove_clones /1,
		save_instance_state /3,
		save_all_instances_state /2,
		load_instance_state /3,
		load_all_instances_state /2,
		remove_instance_state /3,
		remove_all_instances_state /2,
		is_exclusive_action /2,
		is_joint_action /2
	    ]
	).


/** <module>  FRAg Environment Interface

FRAgPL environments Interface. Provides clauses for interaction among agents
and environments

@author Frantisek Zboril
@license GPL
*/


:-include('environments/basic/basic.pl').	% internal actions library



% loaded environments
:-dynamic environment / 1.

%!  agent_environment(Agent, Environment, Instance).
:-dynamic agent_environment /3.




is_exclusive_action(Environment, Action):-
    functor(Action, Term, Arity),
    exclusive_action(Environment, Term, Arity).

is_joint_action(Environment, Action):-
    functor(Action, Term, Arity),
    joint_action(Environment, Term, Arity).



%!  add_environment_library(Module_Name) is det
%   Uses environment specified in Module_Name
%  @arg Module_Name: Path to environment file, relative to fRAg library directory

add_environment_library(Module_Name):-
    current_module(fRAg, FRAg_Path),
    absolute_file_name(Module_Name, Absolute_Module_Name,
                       [relative_to(FRAg_Path)]),
    use_module(Absolute_Module_Name),
    current_module(Environment, Absolute_Module_Name),
    assert(environment(Environment)).

add_environment_library(Module_Name):-
    format("[IFC] Error, environment ~w not loaded~n",[Module_Name]).



%!  set_environment_attributes(+Environment, +Attributes)
%   Set Attributes for Environment. Particular possible attributes are described
%   in Environment doncumentation
%  @arg Environment: Environment identifier
%  @arg Attributes: Environment's environemnt

set_environment_attributes(Environment, Attributes):-
    Environment_Attrs=..[Environment, set_attributes, Attributes],
    !,
    Environment_Attrs.


%!  get_all_environments(-Environments) is det
%   Provides all the available present in the system
%  @arg Environment: List of environments

get_all_environments(Environments):-
    findall(Environment, environment(Environment), Environments).


%!  environment_loaded(+Environment) is semidet
%   Succeed, if Environment has been added to the system
%  @arg Environment: Environment identifier

environment_loaded(Environment):-
    environment(Environment).


%!  clone_environment(+Environment, +Clone) is det
%   Creates a copy of the environment with the identifier Environment_Clone
%   Metody se budou volat jako Envrionment( ..., data ale budou pro kazdy klon
%   samostatne Agent muze bude situovan (situate_agent) do puvodniho prostredi,
%   nebo do klonu (see situate_agent)
%  @arg Envrionment: environment identifier
%  @arg Environment_Clone: ground or free, klon bude odkazovan timto nazvem
%   pouziti pro situovani agentu (jinak si prostredi pohlida, kteremu agentu
%   davat data z originaniho prostredi a kteremu z klonu

clone_environment(Environment, Clone):-
    Clone_Environment=..[Environment, clone, Clone],
    !,
    Clone_Environment.


%!  situate_agent(+Agent:atom, +Environment) is det
%   Situates agent to an environment. Agent will percieve the environment and
%   may act in it
%  @arg Agent: agent name / identifier
%  @arg Envrionment: environment identifier

situate_agent(Agent, Environment):-
    with_mutex(frag_mutex,
       (situate_agent2(Agent, Environment))).


situate_agent2(Agent, Environment):-
    agent_environment(Agent, Environment, Environment),
    format("[IFC] Agent ~w is already situated in environment ~w~n",
            [Agent, Environment]).

situate_agent2(Agent, Environment):-
    environment(Environment),				% does it exist?
    assert(agent_environment(Agent, Environment, Environment)),
    Situate_Agent=..[Environment, add_agent, Agent],
    !,
    Situate_Agent.

situate_agent2(Agent, Environment):-
    format("[IFC] Agent ~w cannot be situated in environment ~w~n",
            [Agent, Environment]).


%!  situate_agent(+Agent, +Environment, +Clone) is det
%   Situates agent to an environment. Agent will percieve the environment and
%   may act in it
%  @arg Agent: agent name / identifier
%  @arg Envrionment: environment identifier
%  @arg Clone: environment Environment clone

situate_agent(Agent, Environment, Clone):-
    environment(Environment),				% does it exist?
    assert(agent_environment(Agent, Environment, Clone)),
    Situate_Agent=..[Environment, add_agent, Agent, Clone],
    !,
    Situate_Agent.

situate_agent2(Agent, Environment, Clone):-
    agent_environment(Agent, Environment, _),
    format("[IFC] Agent ~w cannot be situated in clone ~w of environment ~w,
            it is already situated in an instance of the environment~n",
            [Agent, Clone, Environment]),
    !,
    fail.


%!  get_agent_environments(+Agent, +Environments) is det
%   Provides all the Environments where Agent is situated in
%  @arg Agent: agent name / identifier
%  @arg Environments: list of Environments, where agent is situated in

get_agent_environments(Agent, Environments):-
    findall(Environment, agent_environment(Agent, Environment, _),
          Environments).

get_agent_environments(_, []).



%!  get_agent_instance(+Agent, +Environment, +Clone) is det
%   Provides Clone for an Environment where Agent is sistuated in
%  @arg Agent: agent name / identifier
%  @arg Environment2: One of Environment(s), where Agent is situated in
%  @arg Clone, where the Agent is. If it is the same as Envronment, Agent is in
%   the original instance of Environment

% TODO better instance?
get_agent_instance(Agent, Environment, Instance):-
    agent_environment(Agent, Environment, Instance).



%!  virtualize_agent(+Agent, ?Virtual_Agent) is det
%   For every environment where Agent is situated it creates clone and situates
%   Virtual_Agent there.
%  @arg Agent: agent name / identifier
%  @arg Virtual_Agent: virtualized agent

virtualize_agent_environments(_, []).

virtualize_agent_environments(Agent, [Environment| Environments]):-
    % Environment_Clone the same name as Agent
    clone_environment(Environment, Agent),
    situate_agent(Agent, Environment, Agent),
    virtualize_agent_environments(Agent, Environments).


virtualize_agent(Agent, Agent2):-
    findall(Environment, agent_environment(Agent, Environment, Environment),
          Environments),
    virtualize_agent_environments(Agent2, Environments).

virtualize_agent(_, _).


%!  virtualize_agents(+Agent, ?Virtual_Agents) is det
%   For every environment where Agent is situated creates clone and situates
%   Virtual_Agent there. Name of the clone is the name of the first agent in
%   Virtual_Agents.
%  @arg Agent: agent name / identifier
%  @arg Virtual_Agents: list of agents for virtualization

virtualize_agents(Agent, Virtual_Agents):-
% vytvori klony pro vsechna prostredi, ve kterem je agent
% remove already situated, new virtual agents must not be anywhere
    remove_situated(Virtual_Agents, [Agent2| Agents]),
    virtualize_agent(Agent, Agent2),
% priradi do stejnych klonu zbytek skupiny
    get_agent_environments(Agent2, Environments),
    accompany_agents(Agent2, Agents, Environments).


remove_situated([], []).

remove_situated([Agent| Agents], Agents2):-
    agent_environment(Agent, _, _),
    remove_situated(Agents, Agents2).

remove_situated([Agent| Agents], [Agent| Agents2]):-
    remove_situated(Agents, Agents2).



accompany_agents(_, [], _).

accompany_agents(Agent, [Agent2| Agents], Environments):-
    accompany_agent(Agent, Agent2, Environments),
    accompany_agents(Agent, Agents, Environments).



accompany_agent(_, _, []).

accompany_agent(Agent, Agent2, [Environment| Environments]):-
    agent_environment(Agent, Environment, Clone),
    situate_agent(Agent2, Environment, Clone),
    accompany_agent(Agent, Agent2, Environments).



%
%	Agent AGENT senses ENVIRONMENT and obtainns ADDLIST and DELETELIST
%

%!  agent_perceives(+Agent, +Add_List, +Delete_List) is det
%   For every environment where Agent is situated in creates clone and situates
%   Virtual_Agent there
%  @arg Agent: agent name / identifier
%  @arg Add_List: new percepts for the agent since last perieving
%  @arg Dlete_List: vanished percepts for the agent since last perieving


agent_perceives_environment2(Agent, Environment, Add_List, Delete_List):-
    Percieve_Environment=..[Environment, perceive, Agent, Add_List,
                            Delete_List],
  %  clause(Percieve_Environment, _),			% does it exist?
    !,
%    with_mutex(frag_mutex,(Perceive_Environment)). 
    Percieve_Environment.

agent_perceives_environment(Agent, Environment, Add_List, Delete_List):-
    with_mutex(frag_mutex,
       (agent_perceives_environment2(Agent, Environment, Add_List, Delete_List))).


agent_perceives( _, _, [], []).


agent_perceives2(_, [], [], []):- !.

agent_perceives2(Agent, [Environment | Environments], Add_List_Out,
                 Delete_List_Out):-
    agent_perceives_environment(Agent, Environment, Add_List, Delete_List),
    agent_perceives2(Agent, Environments, Add_List2, Delete_List2),
    append(Add_List, Add_List2,Add_List_Out),
    append(Delete_List, Delete_List2, Delete_List_Out),
    !.



agent_perceives(Agent, Add_List, Delete_List):-
    findall(Environment, agent_environment(Agent, Environment, _),
            Environments),
    !,
    agent_perceives2(Agent, Environments, Add_List, Delete_List).


% no environment assigned
agent_perceives( _, [], []).



%!  agent_acts(+Agent, +Environment, +Act, +Result) is det
%   Agent performs Act in Environment and obtain Result.
%  @arg Agent: agent name / identifier
%  @arg Environment: Environment name, where the Act should be performed
%  @arg Act: Act atom
%  @argResult: Reult is either 'true' in case of successfull exclusive action,
%   an instance of Act in case of joint action, or 'fail', when act fails

agent_acts(Agent, Environment, Act, Result):-
    with_mutex(frag_mutex, 
       (agent_acts2(Agent, Environment, Act, Result))).

% act was not performed in the specified environment

agent_acts( _, _, _, false).


agent_acts2(Agent, Environment, Act, Result):-
    Act_In_Environment =.. [Environment, act, Agent, Act, Result],

    clause(Act_In_Environment, _),			% does it exist?
    Act_In_Environment.



%!  reset_clone(+Environments, +Clone)
%   Reset Clone of an Environments. Reset means that the clones
%   are removed and then are created new of the same name
%  @arg Environment: Environment name
%  @arg Clone: A Clone of the Environment

reset_clone(Environment, Clone):-
    Reset_clone=..[Environment, reset_clone, Clone],
    !,
    Reset_clone.



%!  reset_clones(+Agent)
%   Reset Clones where Agent is situated
%  @arg Agent: Agent name

reset_clones(Agent):-
    get_agent_environments(Agent, Environments),
    reset_clones2(Agent, Environments).


reset_clones2( _, []).

reset_clones2(Agent, [Environment| Environments]):-
    get_agent_instance(Agent, Environment, Environment),
    % agent is in the original instance
    reset_clones2(Agent, Environments).

reset_clones2(Agent, [Environment| Environments]):-
    get_agent_instance(Agent, Environment, Instance),
    reset_clone(Environment, Instance),
    reset_clones2(Agent, Environments).



%!  remove_clone(+Environment, +Clone)
%   Removes all the facts of the clone and registration of agens to the clone
%  @arg Environment: environment name
%  @arg Clone: Clone name

remove_clone(Environment, Clone):-
    Remove_Clone=..[Environment, remove_clone, Clone],
    !,
    Remove_Clone.



%!  remove_clones(+Agent) is det
%   Remove all Clones where Agent is situated
%  @arg Agent: Agent name

remove_clones(Agent):-
    get_agent_environments(Agent, Environments),
    remove_clones2(Agent, Environments).


remove_clones2( _, []).

remove_clones2(Agent, [Environment| Environments]):-
    get_agent_instance(Agent, Environment, Environment),
    % agent is in the original instance
    remove_clones2(Agent, Environments).

remove_clones2(Agent, [Environment| Environments]):-
    get_agent_instance(Agent, Environment, Clone),
    remove_clone(Environment, Clone),
    remove_clones2(Agent, Environments).



%!  save_instance_state(+Environment, +Instance, +State) is det
%   Saves actual state Environment Insance under name State
%  @arg Environment: Environment name
%  @arg Instance: Instance name
%  @arg State: State name

save_instance_state(Environment, Instance, State):-
    Save_State=..[Environment, save_state, Instance, State],
    !,
    Save_State.



%!  save_all_instances_state(+Agent, +State)
%   For every environment instance, where Agent is situated, saves actual State
%  @arg Agent: Agent name
%  @arg State: Name of the states

save_all_instances_state2( _, [], _).

save_all_instances_state2(Agent, [Environment | Environments], State):-
    get_agent_instance(Agent, Environment, Instance),
    save_instance_state(Environment, Instance, State),
    save_all_instances_state2(Agent, Environments, State).

save_all_instances_state(Agent, State):-
    get_agent_environments(Agent, Environments),
    save_all_instances_state2(Agent, Environments, State).



%! load_instance_state(+Environment, +Instance, +State) is semdiet
%   Loads saved State of Environment Insance
%  @arg Environment
%  @arg Instance
%  @arg State

load_instance_state(Environment, Instance, State):-
    Load_State=..[Environment, load_state, Instance, State],
    !,
    Load_State.



%!  load_all_instances_state(+Agent, +State) is det
%   For every environment instance where Agent is situated, loads saved state
%   named State
%  @arg Agent: Agent name
%  @arg State: Name of the states

load_all_instances_state2( _, [], _).

load_all_instances_state2(Agent, [Environment | Environments], State):-
    get_agent_instance(Agent, Environment, Instance),
    load_instance_state(Environment, Instance, State),
    load_all_instances_state2(Agent, Environments, State).

load_all_instances_state(Agent, State):-
    get_agent_environments(Agent, Environments),
    load_all_instances_state2(Agent, Environments, State).



%!  remove_instance_state(+Environment, +Instance, +State)
%   Removes saved State of Environment Instance
%  @arg Environment: Environment name
%  @arg Insrtance: Name of Environment instance
%  @arg State: Name of the state

remove_instance_state(Environment, Instance, State):-
    Remove_State=..[Environment, remove_state, Instance, State],
    !,
    Remove_State.



%!  remove_all_instance_states(+Agent, +State)
%   Removes every environment instance, where Agent is situated
%  @arg Agent: Agent name
%  @arg State: Name of the states

remove_all_instances_state2( _, [], _).

remove_all_instances_state2(Agent, [Environment | Environments], State):-
    get_agent_instance(Agent, Environment, Instance),
    remove_instance_state(Environment, Instance, State),
    remove_all_instances_state2(Agent, Environments, State).

remove_all_instances_state(Agent, State):-
    get_agent_environments(Agent, Environments),
    remove_all_instances_state2(Agent, Environments, State).



:-  initialization(mutex_create(frag_mutex)).

md:-
    use_module(library(pldoc/doc_library)),
%   doc_load_library,
    doc_save('FRAgAgentInterface.pl',
             [format(html), recursive(true), doc_root("../doc")]).


