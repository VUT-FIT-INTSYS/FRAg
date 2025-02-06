            
/*

----------------              --------------
|              |	      |      xxxxxx|
|              |	      | warehouseA |
|              |              |            |
|              |---------------   ---vv-------
|              |              >   |         o|
|              >              |----         o|
| construction |      hall    <  workshop    |
|              <              |---          o|
|              |              >   |         o|
|              |---------------   ---^^-------
|              |              |           xx|
|              |              | warehouseB  |
|	       |	      |  xx  xxx    |
---------------               ---------------

*/




:-module(workshop,
    [
        workshop / 2,
	workshop / 3,
	workshop / 4
    ]
).


/**
@Author Frantisek Zboril jr.
@version 0.1 (2024) 
*/

:- discontiguous workshop/2.
:- discontiguous workshop/3.
:- discontiguous workshop/4.



:-use_module('../FRAgPLEnvironmentUtils').   % interface for environments
:-use_module('../FRAgPLStatsUtils').
:-use_module('../FRAgPLEpisodeSteps').

%   workshop model, state, clauses for model changes, model interface
:-include('workshop_model.pl').


episode(1).
% machine(+Type, +State).
%   @arg Type: unique machine identifier
%   @arg State: true - working, Number - Number of episodes out of order


                          

%!  workshop(++Functionality, +Parameters) is det 
%  @arg Functionality is one of 
%*      set_parameters 
%*      add_agent
%*      clone
%*      reset_clone
%*      remove_clone
%  @arg Attributes: List of parameters in the form of tuples
%   1. For functionality 'set_parameters' the parameters can be
%*      (resources, [(Material,Number)*]).
%*      (resources_max, Resources_Max)
%*	(failure_rate, [Mean, Dispersion]).
%*      (tasks_ratio, Tasks_Rate).
%*      (tasks_max, Tasks_Max).
%   2. For functionality 'add_agant' the parameter is agent's name




%===============================================================================
%                                                                              |
%    WORKSHOP INITALIZATION, MANAGEMENT, CLONING AND SITING AGENTS             |
%                                                                              |
%===============================================================================

%!  init_beliefs(+Agents)
%   Inserts beliefs has / price / sells to Agents
%  @arg Agents: List of agents for which beliefs should be initialized

init_beliefs(Agents):-
    Agents = [Agent | _],	% suppose all the agents are in the same instance
    init_location(Location),
    add_location_percepts(Location, Agent),
    env_utils:findall_environment(workshop, Agent, path( _, _), 
				  Paths),
    env_utils:add_beliefs(Agent, Paths),
    env_utils:add_facts_agent(workshop, Agent, [location(Agent, Location)]),
    env_utils:add_facts_agent(workshop, Agent, [distance_travelled(Agent, 0)]),
    env_utils:add_beliefs(Agent, [location(Location)]),
    env_utils:add_facts_beliefs(workshop, Agent, [episode(1)]),
    env_utils:add_facts_beliefs(workshop, Agent, [stats_(Agent, reward(0))]).


%  !workshop(set_parameters, Parameters) is det
%  @arg Parameters: List of parameters in the form of tuples
%*      (resources, [(Material,Number)*]).
%*      (resources_max, Resources_Max)
%*	(failure_rate, [Mean, Dispersion]).
%*      (tasks_ratio, Tasks_Rate).
%*      (tasks_max, Tasks_Max).
 

workshop(set_parameters, []).

workshop(set_parameters, [(Key, Value)| Parameters]):-
    set_parameter(Key, Value),
    workshop(set_parameters, Parameters).



set_parameter(resources_rates, Resources):-
    retractall(generate_resources_list( _ )),
    assert(generate_resources_list(Resources)).


set_parameter(resources_max, To_Issue):-
    env_utils:delete_facts(workshop, [to_issue( _ )]),
    env_utils:add_facts(workshop, [to_issue(To_Issue)]).


set_parameter(tasks_rate, Tasks_Rate):-
   change_params(tasks_rate(Tasks_Rate)).    

set_parameter(tasks_max, Tasks_Limit):-
   env_utils:delete_facts(workshop, [tasks_limit( _ )]),
   env_utils:add_facts(workshop, [tasks_limit(Tasks_Limit)]).


change_params(Atom):-
   Atom=..[Predicate, _],
   Retract=..[Predicate, _],
   retract(Retract),
   assert(Atom).

change_params(Atom):-
    assert(Atom).



workshop(add_agent, Agent):-
    env_utils:situate_agent_environment(Agent, workshop),
    init_beliefs([Agent]).
    

workshop(add_agent, Agent, Clone):-
    env_utils:situate_agents_clone([Agent], workshop, Clone),
    init_beliefs([Agent]).
    


%!  workshop(add_agent, +Instance +Agent) is det
%   Adds agent Agent to the +Instance of workshop environment
%  @arg Agent: Name of the agent
%  @arg Instance: Insatnce of the workshop environment

workshop(add_agent, Agent, Clone):-
    env_utils:situate_agents_clone([Agent], workshop, Clone),
    init_beliefs([Agent]).


%!  workshop(clone, +Instance) is det
%   Creates a clone 
%  @arg Instance: Insatnce of the workshop environment

workshop(clone, Instance):-
    env_utils:clone_environment(workshop, Instance).


%!  workshop(res6et_clone, +Clone) is det
%   Resets clone to its initial state
%  @arg Clone: workshop environment clone

workshop(reset_clone, Clone):-
    env_utils:reset_environment_clone(workshop, Clone),
    env_utils:get_all_situated(workshop, Clone, Agents),
    init_beliefs(Agents).


%!  workshop(remove_clone, +Clone) is det
%   Removes clone instance
%  @arg Clone:

workshop(remove_clone, Clone):-
    env_utils:remove_environment_clone(workshop, Clone).

workshop(save_state, Instance, State):-
    env_utils:save_environment_instance_state(workshop, Instance, State).

workshop(load_state, Instance, State):-
    env_utils:load_environment_instance_state(workshop, Instance, State).

workshop(remove_state, Instance, State):-
    env_utils:remove_environment_instance_state(workshop, Instance, State).




%===============================================================================
%                                                                              |
%    WORKSHOP PERCEIVING                                                       |
%                                                                              |
%===============================================================================

%!  workshop(perceive, +Agent, -Add_List, -Delete_List) is det
%   Provides environment updates to Agent as Add_List and Delete_List
%  @arg Agent: Agent that perceives some instance of workshop environment
%  @arg Add_List: New percept since last perceiving
%  @arg Delete_List: Disapeared peceps since last perceiving

workshop(perceive, Agent , Add_List, Delete_List):-
    check_episode(Agent),
    !,
    update_location_percepts(Agent),
    env_utils:retreive_add_delete(Agent, Add_List, Delete_List).


check_episode(Agent):-
% druhej workshop ma byt instance ws pro Agenta
    episode_steps:add_agent(Agent, workshop, workshop),
    episode_steps:is_new_episode,
    !,
    query_environment(workshop, Agent, episode(Episode)),
    delete_facts_beliefs_all(workshop, Agent,
                             [episode( Episode )]),
    Episode2 is Episode + 1,
    add_facts_beliefs_all(workshop, Agent, [episode(Episode2)]),
    update_workshop_model(Agent).

check_episode( _ ).

%===============================================================================
%                                                                              |
%    WORKSHOP ACTING	                                                       |
%                                                                              |
%===============================================================================


workshop(act, Agent, go(Location), true):-
    env_utils:query_environment(workshop, Agent, location(Agent, Location2)),
    !,
    env_utils:query_environment(workshop, Agent, path(Location2, Location)),
    env_utils:delete_facts_agent(workshop, Agent, 
			 	 [location(Agent, Location2)]),
    env_utils:add_facts_agent(workshop, Agent, [location(Agent, Location)]),
    env_utils:delete_beliefs(Agent, [location(Location2)]),
    env_utils:add_beliefs(Agent, [location(Location)]).
/*    env_utils:query_environment(workshop, Agent, 
				distance_travelled(Agent, Distance)),

    env_utils:delete_facts_agent(workshop, Agent,
			      [distance_travelled(Agent, Distance2)]),
    Distance2 is Distance + 1,
    env_utils:add_facts_agent(workshop, Agent,
			      [distance_travelled(Agent, Distance2)]).
*/

workshop(act, Agent, go(Location), false):-
    env_utils:query_environment(workshop, Agent, location(Agent, Location2)),
    !,
    not(env_utils:query_environment(workshop, Agent, path(Location2, Location))),

    format("Agent ~w cant go to ~w, there is no path from ~w~n", 
           [Agent, Location, Location2]).


workshop(act, Agent, pick(Machine, Material), true):-
    not(env_utils:query_environment(workshop, Agent, carry(Agent, _))),
    !,
    env_utils:query_environment(workshop, Agent, location(Agent, hall)),
    !,
    env_utils:query_environment(workshop, Agent, product(Machine, Material,
                                                          Number)),
    !,
    Number > 0,
    Number2 is Number - 1,
writeln(produktPicked),
    env_utils:delete_facts_agent(workshop, Agent, [product(Machine, Material,
                                                            Number)]),
    env_utils:add_facts_agent(workshop, Agent, [product(Machine, Material,
                                                         Number2)]),
    env_utils:add_facts_agent(workshop, Agent, 
				[carry(Agent, product(Machine, Material))]),
    env_utils:add_beliefs(Agent, 
				[carry(product(Machine, Material))]).

                                                          
    
workshop(act, Agent, pick(Material), true):- 
    not(env_utils:query_environment(workshop, Agent, carry(Agent, _))),
    !,
    env_utils:query_environment(workshop, Agent, location(Agent, Location)),
    !,
    location_type(Location, warehouse),
    env_utils:query_environment(workshop, Agent, resource(Location, Material,
                                                          Number)),
    !,
    Number > 0,
    Number2 is Number - 1,
    env_utils:delete_facts_agent(workshop, Agent, [resource(Location, Material,
                                                            Number)]),
    env_utils:add_facts_agent(workshop, Agent, [resource(Location, Material,
                                                         Number2)]),
    env_utils:add_facts_agent(workshop, Agent, [carry(Agent,
                                                        resource(Material))]),
    env_utils:add_beliefs(Agent, [carry(resource(Material))]).


workshop(act, Agent, pick(Something), false):- 
    env_utils:query_environment(workshop, Agent, carry(Agent, _)),
    !,
    format("Agent ~w cannot pick ~w, it is carriyng already something~n",
	   [Agent, Something]).




workshop(act, Agent, do(Machine, Material), true):-
    env_utils:query_environment(workshop, Agent, location(Agent, workshop)),
    !,
    env_utils:query_environment(workshop, Agent, machine(Machine, true)),
    !,
    env_utils:query_environment(workshop, Agent, carry(Agent, 
						       resource(Material))),
    env_utils:delete_facts_agent(workshop, Agent, 
                                   [carry(Agent, resource(Material))]),
    env_utils:delete_beliefs(Agent, 
                                   [carry(resource(Material))]),

    env_utils:add_facts_agent(workshop, Agent,
                                [carry(Agent, product(Machine, Material))]),
    env_utils:add_beliefs(Agent,
                                [carry(product(Machine, Material))]),


    machine_used(Machine).



workshop(act, Agent, do(Machine, _), false):- 
    not(env_utils:query_environment(workshop, Agent, machine(Machine, true))),
    !,
    format("Bad luck agent ~w, machine ~w is not available ~n",
	   [Agent, Machine]).




workshop(act, Agent, submit, true):-
    env_utils:query_environment(workshop, Agent, location(Agent, construction)),
    !,
    env_utils:query_environment(workshop, Agent, carry(Agent, 
						       product(Machine, Material))),
    !,
    env_utils:query_environment(workshop, Agent, task(Machine, Material)),
    !,
    env_utils:query_environment(workshop, Agent, stats_(Agent, reward(Reward))),
 
    env_utils:delete_facts_agent(workshop, Agent, 
				   [carry(Agent, product( _, _))]),

    env_utils:delete_beliefs(Agent, [carry(product( _, _))]),
    
    env_utils:delete_facts_beliefs(workshop, Agent, 
				   [stats_(Agent, reward( _ ))]),
    env_utils:delete_facts_beliefs(workshop, Agent, 
				   [task(Machine, Material)]),
    Reward2 is Reward + 10,
    format("Task completed, ~w delivered by ~w~n", [product(Machine, Material), 
                                                  Agent]), 
    env_utils:add_facts_beliefs(workshop, Agent, 
                                   [stats_(Agent, reward(Reward2))]).



workshop(act, Agent, submit, false):-
   env_utils:query_environment(workshop, Agent, carry(Agent, 
						       product(Machine, Material))),
   !,
   not(env_utils:query_environment(workshop, Agent, task(Machine, Material))),
   format("Bad luck, agent ~w, task ~w is not needed now~n",
	  [Agent,task(Machine, Material)]).



%! workshop(act, Agent, drop, Result) is det
%


workshop(act, Agent, drop, true):-
    env_utils:query_environment(workshop, Agent, 
				carry(Agent, resource(Material))),
    !,
    env_utils:query_environment(workshop, Agent, location(Agent, Location)),
    !,
    location_type(Location, warehouse),
    env_utils:delete_facts_agent(workshop, Agent, 
				 [carry(Agent, resource(Material))]),
    env_utils:delete_beliefs(Agent, [carry(resource(Material))]),
    stock_resource(Material, Location, Agent).


stock_resource(Material, Location, Agent):-
    env_utils:query_environment(workshop, Agent, 
				resource(Location, Material, Number)),
    Number2 is Number + 1,
    env_utils:delete_facts_agent(workshop, Agent, [resource(Location, Material,
                                                            Number)]),
    env_utils:add_facts_agent(workshop, Agent, [resource(Location, Material,
                                                         Number2)]).

stock_resource(Material, Location, Agent):-    
    env_utils:add_facts_agent(workshop, Agent, 
			      [resource(Location, Material, 1)]).




workshop(act, Agent, drop, true):-
    env_utils:query_environment(workshop, Agent, 
				carry(Agent, product(Machine, Material))),
    !,
    env_utils:query_environment(workshop, Agent, location(Agent, hall)),
    !,
writeln(produktDroped),
    env_utils:delete_facts_agent(workshop, Agent, 
				   [carry(Agent, product(Machine, Material))]),
    env_utils:delete_beliefs(Agent, [carry(product(Machine, Material))]),
    stock_product(Machine, Material, Agent).




workshop(act, Agent, drop, false):-
  writeln(dropfalse),
  print_workshop_state(Agent).



stock_product(Machine, Material, Agent):-
    env_utils:query_environment(workshop, Agent, 
				product(Machine, Material,Number)),
    Number2 is Number + 1,
    env_utils:delete_facts_agent(workshop, Agent, [product(Machine, Material,
                                                            Number)]),
    env_utils:add_facts_agent(workshop, Agent, [product(Machine, Material,
                                                         Number2)]).

stock_product(Machine, Material, Agent):-    
    env_utils:add_facts_agent(workshop, Agent, 
			      [product(Machine, Material, 1)]).


workshop(act, _, skip, true).


% every other act fails
workshop(act, _, _, false).




:-
    env_utils:register_environment(workshop),
    findall(resource(Location, Material, Number), 
            resource(Location, Material, Number), Facts1),
    findall(machine(Type, State), machine(Type, State), Facts2),
    findall(path(Location1, Location2), path(Location1, Location2), Facts3),
    findall(resource(Location, Material, Number), 
	    resource(Location, Material, Number), Resources),
    findall(product(Machine, Material, Number), 
	    product(Machine, Material, Number), Products),
    to_issue(Material_To_Issue),
    tasks_limit(Tasks_Limit),
    episode(Episode),
    env_utils:add_facts(workshop, Facts1),
    env_utils:add_facts(workshop, Facts2),
    env_utils:add_facts(workshop, Facts3),
    env_utils:add_facts(workshop, Resources),
    env_utils:add_facts(workshop, Products),
    env_utils:add_facts(workshop, [episode(Episode)]),
    env_utils:add_facts(workshop, [to_issue(Material_To_Issue)]),
    env_utils:add_facts(workshop, [tasks_limit(Tasks_Limit)]).

