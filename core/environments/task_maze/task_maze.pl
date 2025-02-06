

:-module(task_maze,
    [
        task_maze / 2,
	task_maze / 3,			
	task_maze / 4			
    ]
).


:-use_module('../FRAgPLEnvironmentUtils').   % interface for environments

:-dynamic dirs /3. % direction from one position to another
:-dynamic item /3. % items in a position

coords(a,[1,1]).
coords(b,[2,1]).
coords(c,[3,1]).
coords(d,[4,1]).
coords(e,[5,1]).
coords(f,[6,1]).
coords(g,[1,2]).
coords(h,[2,2]).
coords(i,[3,2]).
coords(j,[4,2]).
coords(k,[5,2]).
coords(l,[6,2]).
coords(m,[1,3]).
coords(n,[2,3]).
coords(o,[3,3]).
coords(p,[4,3]).
coords(q,[5,3]).
coords(r,[6,3]).
coords(s,[1,4]).
coords(t,[2,4]).
coords(u,[3,4]).
coords(v,[4,4]).
coords(w,[5,4]).
coords(x,[6,4]).
coords(y,[1,5]).
coords(z,[2,5]).
coords(aa,[3,5]).
coords(ab,[4,5]).
coords(ac,[5,5]).
coords(ad,[6,5]).
coords(ae,[1,6]).
coords(af,[2,6]).
coords(ag,[3,6]).
coords(ah,[4,6]).
coords(ai,[5,6]).
coords(aj,[6,6]).



task(gold, gold, gold).

task(gold, gold, bronze).
task(gold, bronze, gold).
task(bronze, gold, gold).

task(gold, silver, bronze).
task(gold, bronze, silver).
% task(silver, gold, bronze).
% task(silver, bronze, gold).
% task(bronze, gold, silver).
% task(bronze, silver, gold).


task(bronze, bronze, silver).
task(bronze, silver, bronze).
% task(silver, bronze, bronze).





% item([2,2], gold).
% item([3,3], gold).
item([1,1], gold, 1).
item([1,2], gold, 2).
item([1,3], gold, 3).
item([1,4], silver, 3).
item([1,5], bronze, 3).
item([1,6], bronze, 3).
item([2,1], silver, 3).
item([2,2], gold, 2).
item([2,3], gold, 2).
item([2,4], gold, 2).
item([2,5], bronze, 1).
item([2,6], silver, 1).
item([3,1], bronze, 1).
item([3,2], bronze, 1).
item([3,3], silver, 2).
item([3,4], silver, 2).
item([3,5], bronze, 1).
item([3,6], gold, 3).
item([4,1], gold, 3).
item([4,2], bronze, 1).
item([4,3], gold, 3).
item([4,4], gold, 3).
item([4,5], silver, 2).
item([4,6], silver, 2).

item([5,1], silver, 2).
item([5,2], bronze, 1).
item([5,3], gold, 3).
item([5,4], silver, 2).
item([5,5], gold, 3).
item([5,6], bronze, 1).

item([6,1], gold, 3).
item([6,2], gold, 2).
item([6,3], gold, 2).
item([6,4], gold, 2).
item([6,5], bronze, 1).
item([6,6], bronze, 1).

size_x(6).
size_y(6).


path_direction([X,Y], left, [X2, Y]):-
    size_x(X_Max),
    X>0,
    X2 is X-1.

path_direction([X,Y], right, [X2, Y]):-
    size_x(X_Max),
    X<X_Max,
    X2 is X+1.

path_direction([X,Y], up, [X, Y2]):-
    size_y(Y_Max),
    Y>0,
    Y2 is Y-1.

path_direction([X,Y], down, [X, Y2]):-
    size_y(Y_Max),
    Y<Y_Max,
    Y2 is Y+1.


 
assert_directions([]).

assert_directions([direction(C1, D, C2)| Directions]):-
    assertz(dirs(C1, D, C2)),
    assert_directions(Directions).  % should be added as environment's facts

init_directions:-
    findall(direction(Coords1,Coords2,D),(coords(_, Coords1), coords(_, Coords2), 
	    path_direction(Coords1, D, Coords2)), Directions),
    add_facts(task_maze, Directions),
    assert_directions(Directions).


init_items:- 
   findall(item(Position, Item, Reward), item(Position, Item, Reward), Items),
   add_facts(task_maze, Items).

init_tasks:-                  
   findall(task(A, B, C), task(A, B, C), Tasks),
   add_facts(task_maze, Tasks).                                             


% get_percepts_doors(Coords, Doors):-
%   findall(door(Direction, Item), (dirs(Coords, Coords2, Direction), 
%                                   item(Coords2, Item, _)), Doors).


%!  get_percepts_position(+Position, -Percepts) is det
%@arg Position: Actual agent's position (room)
%@arg Percepts: List of percepts in that position (room)

get_percepts_position(Position, Agent, [item(Position, Item, Price) | 
					Percepts]):-
    env_utils:findall_environment(task_maze, Agent, 
				  (direction(Position, Position2, Direction)),
                                  Directions),
    assign_items_positions(Agent, Directions, Doors),
    env_utils:query_environment(task_maze, Agent, item(Position, Item, Price)),

%    findall(door(Direction, Item), 
%	    (dirs(Position, Position2, Direction), item(Position2, Item, _)),
%                                  Doors),
    append(Items, Doors, Percepts).


assign_items_positions(_, [],[]).

assign_items_positions(Agent, [direction(_, Position2, Direction)| Directions],
		       [door(Direction, Item)| Doors]):-
    env_utils:query_environment(task_maze, Agent, item(Position2, Item, Price)),
    assign_items_positions(Agent, Directions, Doors).




init_beliefs([]).

init_beliefs([Agent | Agents]):-                          
    init_beliefs_agent(Agent),
    init_beliefs(Agents).


init_beliefs_agent(Agent):-
    env_utils:query_environment(task_maze, Agent, position(Position)),
 %   env_utils:query_environment(task_maze, Agent, tasks(Tasks)),
    findall(task(A, B, C), task(A, B, C), Tasks),
    get_percepts_position(Position, Agent, Percepts),
    add_beliefs_agents([Agent], Tasks),
    add_beliefs_agents([Agent], Percepts).
    

task_maze(add_agent, Agent):-
    situate_agent_environment(Agent, task_maze),
    env_utils:add_facts_beliefs(task_maze, Agent, [bagpoint(1)]),
    env_utils:add_facts(task_maze, [position([1,1])]),
    env_utils:add_beliefs_agents([Agent], [my_position(a)]),
    init_beliefs_agent(Agent).

task_maze(add_agent, Agent, Clone):-
    situate_agents_clone([Agent], task_maze, Clone),
    init_beliefs([Agent]).

task_maze(add_agent, _, _).
 
%    Agent percieves

task_maze(perceive, Agent , Add_List, Delete_List):-
   retreive_add_delete(Agent, Add_List, Delete_List).
 
%    Agent acts
                                
task_maze(act, Agent, go(Direction), true):- 
    query_environment(task_maze, Agent, position(Position)),!,
    path_direction(Position, Direction, New_Position),
    coords(Room, Position),
    coords(New_Room, New_Position),
    delete_facts_agent(task_maze, Agent, [position(Position)]),
    add_facts_agent(task_maze, Agent, [position(New_Position)]),
    delete_beliefs(Agent, [my_position(Room)]),
    add_beliefs(Agent, [my_position(New_Room)]),
    change_room_percepts(Agent, Position, New_Position).

task_maze(act, Agent, go(Direction), false).


task_maze(act, Agent, pick, Result):-
    env_utils:query_environment(task_maze, Agent, position(Position)),!,
    env_utils:query_environment(task_maze, Agent, item(Position, Item, 
				Reward)),!,
    env_utils:query_environment(task_maze, Agent, bagpoint(Pointer)),
    env_utils:delete_facts_beliefs(task_maze, Agent, 
                                   [item(Position, Item, Reward)]),
    degrade_item(Item, Item2, Item2_Price),
    env_utils:add_facts_beliefs(task_maze, Agent, 
                                   [item(Position, Item2, Item2_Price)]),
  
  % TODO
  %  retract(item(Position, Item, Reward)),
  %  assert(item(Position, Item2, Item2_Price)),

    get_result(Pointer, Agent, Item, Result).


task_maze(act, Agent, pick, false).



degrade_item(gold, silver, 2).

degrade_item(silver, bronze, 1).

degrade_item(bronze, dust, 0).

degrade_item(dust, dust, 0).
 
get_result(3, Agent, Item, Result):-
    env_utils:query_environment(task_maze, Agent, bag(1, Item1)),
    env_utils:query_environment(task_maze, Agent, bag(2, Item2)),
   % env_utils:query_environment(task_maze, Agent, tasks(Tasks)),

    findall([A, B, C], task(A, B, C), Tasks),    

    env_utils:delete_facts_beliefs_all(task_maze, Agent, 
         [bag(1, Item1), bag(2, Item2)]),
    env_utils:delete_facts_beliefs(task_maze, Agent, [bagpoint(3)]),
    env_utils:add_facts_beliefs(task_maze, Agent, [bagpoint(1)]),
    get_result2([Item1, Item2, Item], Tasks, Result).
  
get_result(Pointer, Agent, Item, true):-
    env_utils:add_facts_beliefs(task_maze, Agent, [bag(Pointer, Item)]),
    Pointer2 is Pointer +1,
    env_utils:delete_facts_beliefs(task_maze, Agent, [bagpoint(Pointer)]),
    env_utils:add_facts_beliefs(task_maze, Agent, [bagpoint(Pointer2)]).


get_result2(Items, Tasks, reward(1)):-
   check_list_match(Items, Tasks).

get_result2(Items, Tasks, reward(0)).


check_list_match(L1, [L2 | _]):-
   check_list_match2(L1, L2).

check_list_match(L1, [_ | L2]):-
   check_list_match(L1, L2).


check_list_match2(L1, L2):-
 %  msort(L1, L1S),
 %  msort(L2, L2S),
   L1 = L2.

%! change_room_percepts(in Position1, in Position2) is ? 
%   adds from the percept delete list everything it saw in the original room 
%   and adds to the add list what it sees in the new room

change_room_percepts(Agent, Position1, Position2):-
    get_percepts_position(Position1, Agent, Percepts1),
    get_percepts_position(Position2, Agent, Percepts2),
    delete_beliefs(Agent, Percepts1),      
    add_beliefs(Agent, Percepts2).

% Silent actions, clones

task_maze(act, Agent, silently_(go(Direction)), Result):-
    task_maze(act, Agent, go(Direction), Result).
                                            
task_maze(act, Agent, silently_(pick), Result):-
    task_maze(act, Agent, pick, Result).

task_maze(act, _, _, false).      



task_maze(clone, Clone):-
    clone_environment(task_maze, Clone).
    

task_maze(remove_clone, Clone):-
    remove_environment_clone(task_maze, Clone).
 


task_maze(reset_clone, Clone):-
    reset_environment_clone(task_maze, Clone),
    get_all_situated(task_maze, Clone, Agents),   
    init_beliefs(Agents).


task_maze(save_state, Instance, State):-
    save_environment_instance_state(task_maze, Instance, State).


task_maze(load_state, Instance, State):-
    load_environment_instance_state(task_maze, Instance, State).    

task_maze(remove_state, Instance, State):-
    remove_environment_instance_state(task_maze, Instance, State).




:-
    env_utils:register_environment(task_maze),
    init_directions,
    init_tasks,
    init_items.
