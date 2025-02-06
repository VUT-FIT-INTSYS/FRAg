                                                   

%
%  children - 1,list of nodes 2, empty list (final state) 3, non_expanded
%       action model_reasoning_node(goal / wei , plan number, pus) or
% model_act_node(intention, action, decision) - intention is neccessary
% for exclusive actions tree_node(id node, action, children, reward, visited,
% points)
%




%  :-module(fRAgMCTSModel,
%    [
%	mcts_model_init /0,
%	mcts_expand_node /2,
%	mcts_increment_path /2,
%	mcts_get_best_ucb_path /2,
%       mcts_divide_path /3,
%	mcts_print_model /1,
%	mcts_print_path /2
%    ]
%  ).



:-thread_local fresh_node_index/1.

% tree_node(Node_ID, Act, Children, Node_Reward, Visits, Score).
:-thread_local tree_node/6.

:-thread_local root_node/1.


fresh_node_index(1).



set_root_node(Root):-
    retractall(root_node(_)),
    assert(root_node(Root)).


mcts_print_all:-
   bagof(tree_node(A,B,C, Reward, D,E),tree_node(A,B,C, Reward, D,E),L).

mcts_print_model(Debug):-
    root_node(Root),
    model_print_node(Root,' - ', Debug).

mcts_print_model( _ ).


model_print_node(Node_ID, Sufix_String, Debug):-
    tree_node(Node_ID, Act, Children, Reward, Visits, Score),
%    print_debug(Sufix_String, Debug),
%   format(atom(Tree_NodeS), "~w~w", [Sufix_String, tree_node(Node_ID, Act,
%				      Children, Visits, Score)]),
%    ValueX2 is div(Score, Visits),

   count_reward_value(Value, Score, Visits),
   format(atom(Tree_NodeS), "~w(~w)[~3f/~w:~3f/REW:~w] ~w",
                              [Sufix_String, Node_ID, Score, Visits,  
				Value, Reward, Act]),
    println_debug(Tree_NodeS, Debug),
%   format(atom(Tree_NodeCh), "4~wchildren:~w",
%                              [Sufix_String, Children]),
%    println_debug(Tree_NodeCh, Debug),
    format(atom(Sufix_String2), "~w - ", [Sufix_String]),
    model_print_node_children(Children, Sufix_String2, Debug).


count_reward_value(0, Score, 0).

count_reward_value(Value, Score, Visits):-
    Value is Score / Visits.



model_print_node_children(not_expanded, _, _).

model_print_node_children([], _, _).

model_print_node_children([NodeChild|T], SfxStr, Debug):-
    model_print_node(NodeChild,SfxStr, Debug),
    model_print_node_children(T,SfxStr, Debug).





get_fresh_node_id(Index):-
    fresh_node_index(Index),
    retract(fresh_node_index(_)),
    Index2 is Index+1,
    assert(fresh_node_index(Index2)).




%!  generate_children(+Index_starts, +Index_ends, -Children) is det
%   vytvori seznam indexu od Index_starts do Index_ends,
%  @arg Index_starts: integer


generate_children(Index, Index, []).

generate_children(Start_ID, End_ID, [Start_ID| Indexes]):-
    Start_ID2 is Start_ID +1,
    generate_children(Start_ID2, End_ID, Indexes).


%!  set_children(+Node_index: ke kteremu uzlu se pripojuji decka, +Index_starts, Index_Ends) is det
%   @todo
%  @arg Index_starts: integer


set_children(Node_ID, Start_ID, End_ID):-
    format(atom(ChildrenS), "Generate children ~w , ~w", [Start_ID, End_ID]),
    println_debug(ChildrenS, mctsdbg),
    generate_children(Start_ID, End_ID, Children_IDs),
    retract(tree_node(Node_ID, Act, _, Reward, Visits, Score)),
    assert(tree_node(Node_ID, Act, Children_IDs, 1, Visits, Score)).

expand_node2([]).

expand_node2([Element| Elements]):-
    get_fresh_node_id(ID),
    assert(tree_node(ID, Element, not_expanded, 0, 0, 0)),
    expand_node2(Elements).



%!  expand_node(+Node_index: integer, Elements: list of nodes) is det
%   creates nodes for elements and assign them to a parent node
%   * Node_index
%       index of parent node
%   * Elements
%	elements to be assigned to the parent node

mcts_expand_node(Node_ID, Elements):-
    fresh_node_index(Start_ID),
    expand_node2(Elements),
    fresh_node_index(End_ID),
    set_children(Node_ID, Start_ID, End_ID).


% true, pokud je  not_expanded

expand_candidate(Parent_ID, Parent_ID):-
    tree_node(Parent_ID, _, not_expanded, _, _, _).



%! ucb(+Parent_ID, +Child_ID, -UCB) is det
% for a list of children (4th term in tree_node) finds out their 
% ucb's -> binded to UctList
% wi/ni + sqrt(2)*sqrt((ln Ni) / ni)
% wi - wins of the node, ni - runs over the node, Ni runs over parent's node
% (after ith step)
%@arg Parent_ID:
%@arg Child_ID:
%@arg UCB: Upper Confidence Bound

ucb(Parent_ID, Child_ID, UCB):-
    mcts_max_reward(Max_Reward),
    tree_node(Parent_ID, _, _, Reward1, Parent_Visits, _),
    tree_node(Child_ID, _, _, Reward2, Child_Visits, Child_Value),
 %   UCB is (Child_Value/Child_Visits) + 
    UCB is Child_Value / Max_Reward + sqrt(2*(log(Parent_Visits)/Child_Visits)).



%
% Increment visited / result on path
%
% TODO asi by stacil na vstupu jen INDEX, zbytek se vytahne v retractu


increment_node(tree_node(Node_ID, Act, Children, _, Visits, Value), 
			 Reward, Value2):-
 % TD Learning: V(st) = (1-alpha)*V(st) + alpha*(Rewardt' + gamma*(V(st'))
    mcts_gamma(Gamma),
    mcts_alpha(Alpha),
    retract(tree_node(Node_ID, Act, Children, _, Visits, Value)),
    Visits2 is Visits + 1,
    Value3 is ((1-Alpha)*Value + Alpha*(Reward + Gamma*Value2)),
    format("Node ~w upgradet from value ~w to value ~w, next value was ~w and reward ~w alpha:~w, gamma:~w~n",
		[Node_ID, Value, Value3, Value2, Reward, Alpha, Gamma]),
    assert(tree_node(Node_ID, Act, Children, Reward, Visits2, Value3)).



mcts_propagate_results([leaf_node(ID), _], [], Value2):-      % The last one
   tree_node(ID, Act, Children, Reward, Visits, Value),
   increment_node(tree_node(ID, Act, Children, Reward, Visits, Value),
		   0, Value2).



mcts_propagate_results([leaf_node(ID), _], [Reward], Value2):-      % The last one
   tree_node(ID, Act, Children, Reward, Visits, Value),
   increment_node(tree_node(ID, Act, Children, Reward, Visits, Value),
		   Reward, Value2).


mcts_propagate_results([node(ID), _], [], Value2):-      % The last one
   tree_node(ID, Act, Children, Reward, Visits, Value),
   increment_node(tree_node(ID, Act, Children, Reward, Visits, Value),
		   0, Value2).


mcts_propagate_results([leaf_node(ID), _ | Nodes], [Reward | Rewards], Value2):-
   tree_node(ID, Act, Children, _, Visits, Value),
   increment_node(tree_node(ID, Act, Children, _, Visits,  Value), 
		  Reward, Value2),
   tree_node(ID, _, _, _, _, Value3),
   mcts_propagate_results(Nodes, Rewards, Value3).



mcts_propagate_results([node(ID), _| Nodes], [Reward | Rewards], Value2):-
 format("Proresults ~w~nRewards ~w~n",[[node(ID)|Nodes], [Reward|Rewards]]),
    tree_node(ID, Act, Children, _, Visits, Value),
    increment_node(tree_node(ID, Act, Children, _, Visits, Value), 
		   Reward, Value2),
    tree_node(ID, _, _, _, _, Value3),
    mcts_propagate_results(Nodes, Rewards, Value3).



mcts_print_path([], _).

mcts_print_path([Node_ID, Node| Path], Debug):-
    format(atom(PathS)," - ~w : ~w", [Node_ID, Node]),
%    print_debug(' - ', mctsdbg_path),
%    print_debug(Node_ID, Debug),
%    print_debug(':', mctsdbg_path),
    println_debug(PathS, Debug),
    mcts_print_path(Path, Debug).



%!  mcts_get_best_ucb_path(-Path, +UCB) is det
%@arg Path: best path due to criteria below
%@arg UCB: if true, the criterion is the UCB value, false - the criterion is
%           the exploitation rating of the node

mcts_get_best_ucb_path(Path, UCB):-
    root_node(Root),
    !,
    mcts_get_best_ucb_path(Root, Path, UCB).

mcts_get_best_ucb_path(ID, [leaf_node(ID), Action], _):-
    tree_node(ID, Action, not_expanded, Reward, _, _).

mcts_get_best_ucb_path(ID, [leaf_node(ID), Action], _):-
    tree_node(ID, Action, [], Reward, _, _).

mcts_get_best_ucb_path(ID, [node(ID), Action| Path], UCB):-
    tree_node(ID, Action, Children, Reward, _, _),
    select_best_child(ID, Children, Best_Child, UCB),
    mcts_get_best_ucb_path(Best_Child, Path, UCB).



% best child of the node
%     UCB = true  ... depends on UCB   (for making MCTS model)
%     UCB = false ... depends on score (for extraction of the best path of the model)

select_best_child( _, [Child| _], Child, _):-
    tree_node(Child, _, not_expanded, Reward, _, _).


select_best_child(Parent, [Child| Children] , Best_Child, UCB):-
    select_best_child(Parent, Children, Best_Child2, UCB),
    select_best_child2(Parent, Child, Best_Child2, Best_Child, UCB).
 
select_best_child(_, [Child], Child, _).



%  select_best_child2(Parent, Child1, Child2, Child, false) ?? TODO

/*
select_best_child2(_ , Child, _, Child, _):-
 writeln(sbch21a),
 writeln(Child),
    tree_node(Child, _, not_expanded, _, _),
 writeln(sbch21b).
*/

select_best_child2(_ , _, Child, Child, _):-
    tree_node(Child, _, not_expanded, _, _, _).

select_best_child2( _, Child1, Child2, Child, false):-
    tree_node(Child1, _, _, Reward1, Visits1, Score1),
    tree_node(Child2, _, _, Reward2, Visits2, Score2),
    Success1 is Score1, % / Visits1,
    Success2 is Score2, % / Visits2,
    select_best_child3(Success1, Child1, Success2, Child2, Child).

select_best_child2(Parent, Child1, Child2, Child, true):-
   ucb(Parent, Child1, UCB1),
   ucb(Parent, Child2, UCB2), !,
   select_best_child3(UCB1, Child1, UCB2, Child2, Child).


select_best_child3(Value1, Child1, Value2, _, Child1):-
    Value1 > Value2.

select_best_child3( _, _, _, Child2, Child2).




  % select_best_child(Parent, List of children, Child, UCB)




%
%  MCTS supporting clauses
%

%!  divide_path(+PATH: list of nodes, -REASONING_PREFIX: list of nodes, -FIRST_ACT: node) is det
%   rozdeli path na prefix obsahujici reasoning nodes na zacatku az po prvni akt, a dale onen akt
%   * PATH
%       List of nodes, path in the model
%   * REASONING_PREFIX
%       List of reasoning nodes before the first act node
%   * -FIRST_ACT
%       The first act node in the path



divide_path2([_,model_reasoning_node(Event, Plan, Context)| Path],
             [model_reasoning_node(Event, Plan, Context)| Reasoning_Nodes],
             Act):-
    divide_path2(Path, Reasoning_Nodes, Act).

divide_path2([ _, model_act_node(Intention, Action, Context)| _], [],
             [model_act_node(Intention, Action, Context)]).

divide_path2(_,[],[]).

% PATH - optimal path by MCTS
	% REASONING - path prefix (without the Root node) of reasoning nodes before the first act
	% ACT - list with the first act in PATH

mcts_divide_path([_,_| Path], Reasoning_Nodes, Act_Node):-
    divide_path2(Path, Reasoning_Nodes, Act_Node).


% model_init:-
%   fresh_node_index(_),
%   tree_node(root, _, _, _, _, _).



mcts_model_init:-
    retractall(fresh_node_index(_)),
    retractall(root_node(_)),
    retractall(tree_node( _, _, _, _, _, _)),
    garbage_collect,
    assert(root_node(root)),
    assert(fresh_node_index(1)),
    assert(tree_node(root, model_act_node(no_intention, no_action, [[]]),
                     not_expanded, 2, 0, 0)).


                                        