

:- dynamic resource /3.
:- dynamic to_issue /1.
:- dynamic generate_resources_list /1.
:- dynamic task/2.
:- dynamic tasks_rate /1.
:- dynamic to_issue /1.
:- dynamic tasks_limit /1.
% :- dynamic distance_travelled /2.


path(construction, hall).
path(hall, warehouseA).
path(hall, warehouseB).
path(warehouseA, workshop).
path(warehouseB, workshop).
path(workshop, hall). 
path(hall, construction).

location_type(warehouseA, warehouse).
location_type(warehouseB, warehouse).
location_type(construction, construction).
location_type(hall, hall).
location_type(workshop, workshop).


machine(machine1, true).
machine(machine2, true).
machine(machine3, true).
machine(machine4, true).


product(machine1, plactic,1).
/*
product(machine1, stone,1).
product(machine1, metal,1).
product(machine2, wood,1).
product(machine2, glass,1).
product(machine3, plactic,1).
product(machine3, stone,1).
product(machine3, metal,1).
product(machine4, wood,1).
product(machine4, glass,1).
product(machine4, metal,1).
product(machine4, stone,1).
product(machine1, wood,1).
product(machine1, glass,1).
% product(machine2, plactic,1).
% product(machine2, stone,1).
% product(machine2, metal,1).
% product(machine4, plactic,1).
% product(machine3, glass,1).
% product(machine3, wood,1).
*/



resource(warehouseA, plastic, 0).
resource(warehouseA, stone, 0).
resource(warehouseA, wood, 0).
resource(warehouseA, metal, 0).
resource(warehouseA, glass, 0).

resource(warehouseB, plastic, 0).
resource(warehouseB, stone, 0).
resource(warehouseB, wood, 0).
resource(warehouseB, metal, 0).
resource(warehouseB, glass, 0).

               


task(glass, machine1).

% limit for issuing material
to_issue(20).
tasks_limit(20).

init_location(construction).



generate_resources_list([]).
tasks_rate(5).
machine_adjustment_rate(8).


%!  update_workshop_model is det
%   Updates models ... add resources and tasks, updates machines

update_workshop_model(Agent):-
    generate_resources_list(Resources),
    generate_resources(Resources, warehouseA, Agent),
    !,
    generate_resources(Resources, warehouseB, Agent),
    !,
    generate_tasks(Agent),
    repair_machines(Agent).
 %   print_workshop_state(Agent).


%!  machine_used(Machine) is nondet
%   Sets the time for repair and adjustment of the machine after its use
%  @arg Machine: name of used machine

machine_used(Machine):-
    machine_adjustment_rate(Adjustment_Rate),
    frag_stats:poisson_dist_sample(Adjustment_Rate, Number),
% at least one episode step repair
    Number2 is Number+1,
    env_utils:delete_facts_agent(workshop, Agent, [machine(Machine, _)]),
    env_utils:add_facts_agent(workshop, Agent, [machine(Machine, Number2)]).


repair_machines(Agent):-
    env_utils:findall_environment(workshop, Agent, machine( _, _), Machines),
    repair_machines(Agent, Machines).


%!  repair_machines(Machines) is det
%   In each episodic step the repair time is decremented for machines in repair
%  @arg Machines: list of all machines in the model


repair_machines( _, []).

% the 'head' machine is ok
repair_machines(Agent, [machine( _, true)| Machines]):-
    repair_machines(Agent, Machines).

% the 'head' machines is about to be ok
repair_machines(Agent, [machine(Machine, 1)| Machines]):-
    env_utils:delete_facts_beliefs(workshop, Agent, [machine(Machine, 1)]),
    env_utils:add_facts_agent(workshop, Agent, [machine(Machine, true)]),
    repair_machines(Agent, Machines).

% the 'head' machines is under repair
repair_machines(Agent, [machine(Machine, Number)| Machines]):-
    Number1 is Number - 1,
    env_utils:delete_facts_beliefs(workshop, Agent, [machine(Machine, Number)]),
    env_utils:add_facts_agent(workshop, Agent, [machine(Machine, Number1)]),
    repair_machines(Agent, Machines).



print_workshop_state(Agent):-
    nl,
    env_utils:findall_environment(workshop, Agent, resource(_, _, _), 
				  Resources),
    print_list(Resources),
    env_utils:findall_environment(workshop, Agent, task(_, _), Tasks),
    print_list(Tasks),
    env_utils:findall_environment(workshop, Agent, location(_, _), 
				  Locations),
    print_list(Locations),
    env_utils:findall_environment(workshop, Agent, product(_, _, _), 
				  Products),
    print_list(Products),

    env_utils:findall_environment(workshop, Agent, carry(_, _), 
				  Carries),
    print_list(Carries),


    env_utils:findall_environment(workshop, Agent, stats_(_, reward( _ )), 
				  Rewards),
    print_list(Rewards),
    env_utils:findall_environment(workshop, Agent, distance_travelled(_, _), 
				  Distances),
    print_list(Distances).


    
print_list([]).

print_list([Item | Items]):-
    writeln(Item),
    print_list(Items).


%!  geneerate_tasks(Agent) is nondet


generate_tasks(Agent):-
    tasks_rate(Tasks_Rate),
    frag_stats:poisson_dist_sample(Tasks_Rate, Number),
    check_tasks_limit(Agent, Number, Number2),
    generate_tasks(Number2, Agent).

generate_tasks( _ ):- writeln(chyba).



generate_tasks(0, _).

generate_tasks(N, Agent):-
    findall(m(Machine), env_utils:fact(workshop, workshop, machine(Machine, _)), 
                                       Machines),
    findall(r(Resource), env_utils:fact(workshop, workshop, 
            resource(_, Resource, _)), Resources),
    random_member(m(Machine2), Machines),
    random_member(r(Resource2), Resources),  
    env_utils:add_facts_agent(workshop, Agent, [task(Machine2, Resource2)]),
    N2 is N-1,
    generate_tasks(N2, Agent).

 


check_tasks_limit(Agent, Number, Number2):-
    env_utils:query_environment(workshop, Agent, tasks_limit(Limit)),
    check_tasks_limit(Number, Number2, Limit, Limit2),
    env_utils:delete_facts_agent(workshop, Agent, [tasks_limit(Limit)]),
    env_utils:add_facts_agent(workshop, Agent, [tasks_limit(Limit2)]).


check_tasks_limit(Number, Number2, Limit, 0):-
    Number>Limit,
    Number2 is Limit.

check_tasks_limit(Number, Number2, Limit, Limit2):-
    Number2 is Number,
    Limit2 is Limit - Number.




% for Agent
generate_resources([], _, _).

generate_resources([Resource| Resources], Warehouse, Agent):-
    generate_resource(Resource, Warehouse, Agent),
    generate_resources(Resources, Warehouse, Agent).

generate_resource(resource(Material, Lambda), Warehouse, Agent):-
    frag_stats:poisson_dist_sample(Lambda, Number),
    env_utils:query_environment(workshop, Agent, to_issue(Number_Max)),
    Number2 is min(Number, Number_Max),
    Number3 is Number_Max - Number2,
    env_utils:query_environment(workshop, Agent, resource(Warehouse, Material, Number4)),
    env_utils:delete_facts_agent(workshop, Agent, 
				   [to_issue(Number_Max),
			   	    resource(Warehouse, Material, Number4)]),
    Number5 is Number4 + Number2,
    env_utils:add_facts_agent(workshop, Agent, 
			       [to_issue(Number3),
 				resource(Warehouse, Material, Number5)]).

   



%	PLACE PERCEPTS - ADD/DELETE WHEN ROBOT CHANIGES PLACE

update_location_percepts(Agent):-
    env_utils:query_environment(workshop, Agent, location(Agent, Location)),
    delete_location_percepts(Location, Agent),
    add_location_percepts(Location, Agent).


add_location_percepts(Location, Agent):-
    get_location_percepts(Location, Agent, Percepts),
    env_utils:add_beliefs(Agent, Percepts).



%  delete all the percepts about machines, tasks, resources and products

delete_location_percepts( _ , Agent):-
     env_utils:delete_beliefs(Agent, [task( _, _), machine( _, _), 
			 	      resource( _, _, _), product( _, _, _)]).



% new tasks observed at 
get_location_percepts(construction, Agent, Percepts):-
    env_utils:findall_environment(workshop, Agent, task( _, _), 
				  Percepts).


% machines and their states
get_location_percepts(workshop, Agent, Percepts):-
    env_utils:findall_environment(workshop, Agent, machine( _, _), 
				  Machines),
    extract_machines(Machines, Percepts).


% resources
get_location_percepts(Warehouse, Agent, Percepts):-
    location_type(Warehouse, warehouse),
    env_utils:findall_environment(workshop, Agent, resource(Warehouse, _, _), 
				  Resources),
    extract_resources(Resources, Percepts).


% products in hall
get_location_percepts(hall, Agent, Percepts):-
    env_utils:findall_environment(workshop, Agent, product( _, _, _), 
				  Products),
    extract_products(Products, Percepts).


get_location_percepts(_, _, []).



extract_products([], []).

extract_products([product(_, _, 0) | Products], 
		 Percepts):-
    extract_products(Products, Percepts).

extract_products([product(Machine, Material, Number) | Products], 
		 [product(Machine, Material, Number) | Percepts]):-
    extract_products(Products, Percepts).



extract_machines([], []).

extract_machines([machine(Machine, State) | Machines], 
		 [machine(Machine, State) | Percepts]):-
    extract_machines(Machines, Percepts).

extract_machines([ _ | Machines], 
		 Percepts):-
    extract_machines(Machines, Percepts).

     

extract_resources([], []).

extract_resources([resource(_, _, 0) | Resources], 
		 Percepts):-
    extract_resources(Resources, Percepts).

extract_resources([resource(Location, Material, Number) | Resources], 
		 [resource(Location, Material, Number) | Percepts]):-
    extract_resources(Resources, Percepts).




get_place_percepts(warehouse, _, []).


