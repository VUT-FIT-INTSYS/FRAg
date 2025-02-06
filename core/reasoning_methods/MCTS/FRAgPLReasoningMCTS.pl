                                                             

/**

This file is part of the FRAg program. It is insluded into agent's file
FRAgAgent.pl. It contains clauses that are applied to strategies for selecting
intentions, plans and substitutions. MCTS reasoning performs selection of
actions, plans and substitutions based on Monte Carlo Tree Search simulations.

@author Frantisek Zboril
@version 2022 - 2024
@license GPL

*/


% This module is loaded / included in the FRAgAgent file

%  :- use_module('FRAgMCTSModel').

:-dynamic simulate_late_bindings/1.		% should do the tree for early bindings (expand individual reasonings and tests?)
:-dynamic mcts_default_expansions/1.
:-dynamic mcts_default_number_of_simulations/1.
:-dynamic mcts_simulation_steps/1.
% based on model, either greedy, or ucb
:-thread_local recomended_path/2.
:-thread_local mcts_expansions/1.
:-thread_local mcts_number_of_simulations/1.



:-include('FRAgMCTSModel.pl').  % internal actions library


%
%	Static atoms
%

mcts_default_expansions(10).
mcts_default_number_of_simulations(10).
mcts_simulation_steps(100).

reasoning_method(mcts_reasoning).


mcts_gamma(1).
mcts_alpha(0.25).
mcts_max_reward(3).

% actual path found by last mcts execution, first is reasoning prefix,
% then action?

recomended_path([],[]).

% signalizes that mcts model is needed to mantain during agent's execution
% for this type of reasoning


%!  set_reasoning_method_params(Parameters) is det
%  @arg Parameters: expected mcts_params(+Expansions, +Simulations, +Steps)
%  @Expansions: Number of expansions during model update
%  @Simulations: Number of rollout simulations per expansion
%  @Steps: Maximal number of loops/steps in simulation, when agent does not
%   finish, then is aborted by force after Steps loops

set_reasoning_method_params(mcts_params(Expansions, Simulations, Steps)):-
    retractall(mcts_default_expansions( _ )),
    retractall(mcts_default_number_of_simulations( _ )),
    retractall(mcts_simulation_steps( _ )),
    assert(mcts_default_expansions(Expansions)),
    assert(mcts_default_number_of_simulations(Simulations)),
    assert(mcts_simulation_steps(Steps)).


%
%  tree -> act, children acts
%  act -> plan selection for an intentin od plan act
%




%
% Reasoning redefined here ... gets all options for all the goals
%
% 

get_decisions(PUS, Variables, Decisions):-
    shorting_pus(PUS, Variables, Decisions).

% ??
perform_reasoning( _, _):-
    true.



%
% expand actions -> gets all possible actions + decisions
%


% get_actions ... add, del, act (with decisionings) test, ach (just goals,
% no context selection/decisions)

get_all_action_decisions2(_, _, [], []).

get_all_action_decisions2(Intention_Index, Action, [Decision| Decisions],
                          [model_act_node(Intention_Index, Action, [Decision])|
                              Acts]):-
    get_all_action_decisions2(Intention_Index, Action, Decisions, Acts).


% Action is ground
get_all_action_decisions(Intention_Index, Action, _,
                         [model_act_node(Intention_Index, Action,[[]])])
    :-
    term_variables(Action, []).


get_all_action_decisions(Intention_ID, Action, Context, Actions):-
    term_variables(Action, Variables),
    get_decisions(Context, Variables, Decisions),
%	write("decisions: "), write(get_decisions(CONTEXT, VARS, DECISIONS)),
    get_all_action_decisions2(Intention_ID, Action, Decisions, Actions).


% For late bindings goals actions (test, ach) are abstracted without decisions
% in the case of early bindings, must be also expanded for individual decisions

get_actions(Intention_Index, test(Goal), _,
            [model_act_node(Intention_Index, test(Goal),[[]])])
    :-
    simulate_late_bindings(true).


get_actions(Intention_Index, test(Goal), Context, Actions):-
% test goal is set with its results, should be simulated
    bagof(fact(Goal), fact(Goal), Test_Goals),
    broad_unification(fact(Goal), Test_Goals, Context2),
    restriction(Context, Context2, Context3),
    get_all_action_decisions(Intention_Index, test(Goal), Context3, Actions).


% tato akce ma spadnout, takze nechame test goal jak byl (i kdyby nahodou ve
% skutecnosti prosel)
get_actions(Intention_Index, test(Goal), _,
            [model_act_node(Intention_Index, test(Goal),[[]])]).
% test goal is set with its results, should be simulated


get_actions(Intention_Index, rel(Relation), _,
            [model_act_node(Intention_Index, rel(Relation),[[]])]).
% test goal is set with its results, should be simulated


% case of late bindings
get_actions(Intention_Index, ach(Goal),_ ,
            [model_act_node(Intention_Index, ach(Goal),[[]])]):-
    simulate_late_bindings(true).

get_actions(Intention_ID, ach(Goal), Context, Actions):-
    get_all_action_decisions(Intention_ID, ach(Goal), Context, Actions).

get_actions(Intention_ID, tlg(Goal), Context, Actions):-
    get_all_action_decisions(Intention_ID, tlg(Goal), Context, Actions).

get_actions(Intention_ID, add(Goal), Context, Actions):-
    get_all_action_decisions(Intention_ID, add(Goal), Context, Actions).

get_actions(Intention_ID, del(Goal), Context, Actions):-
    get_all_action_decisions(Intention_ID, del(Goal), Context, Actions).

get_actions(Intention_ID, act(Goal), Context, Actions):-
    get_all_action_decisions(Intention_ID, act(Goal), Context, Actions).

get_actions(Intention_ID, act(Environment, Goal), Context, Actions):-
    get_all_action_decisions(Intention_ID, act(Environment, Goal), Context,
                             Actions).


% expand state -> all possible actions

model_expand_actions2([],[]).   % no intention left, no actions

model_expand_actions2([intention(Intention_ID ,
                                [plan( _, _, _, _, _, [])| _],
                                active) |
                          Intentions],
                      [model_act_node(Intention_ID, true, [])| Acts]
                     ):-
    model_expand_actions2(Intentions, Acts).

model_expand_actions2([intention(Intention_ID ,
                                 [plan( _, _, _, _, Context, [Act | _])| _],
                                       active) | Intentions], Acts3):-
    get_actions(Intention_ID, Act, Context, Acts),
    format(atom(String),"[MCTS] +++ Actions to expand: ~w", [Acts]),
    println_debug(String, mctsdbg),
    sort(Acts, Acts_Sorted),    % remove duplicates
    model_expand_actions2(Intentions, Acts2),
    append(Acts_Sorted, Acts2, Acts3).


model_expand_actions(Actions):-
    bagof(intention(Intention_ID,Plan_Stack, Status),
          intention(Intention_ID, Plan_Stack, Status), Intentions),
    model_expand_actions2(Intentions, Actions),
    format(atom(String),"[MCTS] Expanded actions: ~w",[Actions]),
    println_debug(String, mctsdbg),
    temp_pa(Actions).
 
model_expand_actions([]).   % in the case there is no intention

temp_pa([]).
temp_pa([H|T]):-
 temp_pa(T).


%
% expand state -> all possible deliberations
%
%!  model_expand_deliberations(
%TODO


model_expand_deliberations(Deliberations):-
    bagof(event(Event_ID, Type,Predicate,Intention, Context,active, History),
	  event(Event_ID, Type,Predicate,Intention, Context,active, History),
	  Events),
    !,
    model_expand_deliberations2(Events, Deliberations).

model_expand_deliberations([]).   % in the case there is no goal



model_expand_deliberations2([], []).

model_expand_deliberations2([event(Event_ID, Type, Predicate,Intention,
                                   Context, State, History)| TEVENTS], 
                            Deliberations)
    :-
    get_relevant_applicable_plans(Type, Predicate, Context, RAPlans),
    format(atom(RAPlansS), "Expand RA, RAPlans: ~w", [RAPlans]),
    println_debug(RAPlansS, mctsdbg),
    model_expand_deliberations3(event(Event_IT, Type, Predicate, Intention,
                                Context, State, History), RAPlans, 
                                RAPlans_elements),
    model_expand_deliberations2(TEVENTS, RAT),
    append(RAPlans_elements, RAT, Deliberations).

  % no RE plans
model_expand_deliberations2([event( _, _, _, _, _, _, _)| Events], MEANS):-
        model_expand_deliberations2(Events, MEANS).



model_expand_deliberations3(_,[],[]).

model_expand_deliberations3(Goal,[[plan(Plan_ID, Event_Type, Event_Atom,
                                        Conditions, Body), Context]| Plans],
                            Deliberation_Nodes):-
    model_expand_deliberations4(Goal,[plan(Plan_ID, Event_Type, Event_Atom,
                                           Conditions, Body), Context], MDNT1),
    model_expand_deliberations3(Goal, Plans, MDNT2),
    append(MDNT1,MDNT2,Deliberation_Nodes).



model_expand_deliberations4(Goal,
                           [plan(Plan_ID, Goal_Type, Goal_Atom, Conditions ,
                                 Plan_Body), Context],
                           [model_reasoning_node(Goal,
                                                 plan(Plan_ID,Goal_Type,
                                                      Goal_Atom, Conditions,
                                                      Plan_Body),
                                                 Context)]):-
    simulate_late_bindings(true).

model_expand_deliberations4(_,[_,[]], []).

model_expand_deliberations4(Goal, [plan(PlanNumber,PlanType,PlanGoal,PlanGuards,PlanBody),[Unif| T]],
    [model_reasoning_node(Goal, plan(PlanNumber,PlanType,PlanGoal,PlanGuards,PlanBody), [Unif])| MDNT]):-
    model_expand_deliberations4(Goal, [plan(PlanNumber,PlanType,PlanGoal,PlanGuards,PlanBody),T], MDNT).



% Silence program / plan ... translates plan such that all act(ACT) in the
% program plans which are not relations or 'is' are translated to act(__foo(ACT))
% such act is not executed, but the decision on the variables in ACT is kept
%


silence_program([],[]).

silence_program(
    [plan(Plan_ID, Goal_Type, Goal_Atom, Conditions, Context, Acts)| Plans],
    [plan(Plan_ID, Goal_Type, Goal_Atom, Conditions, Context, Acts2)| Plans2]):-
    silence_plan(Acts,Acts2),
    silence_program(Plans,Plans2).

silence_program(
    [plan(Goal_Type, Goal_Atom, Conditions, Context, Acts)| Plans],
    [plan(Goal_Type, Goal_Atom, Conditions, Context, Acts2)| Plans2]):-
    silence_plan(Acts,Acts2),
    silence_program(Plans,Plans2).


silence_program([intention(A,Plans,C)|T], [intention(A,Plans2,C)|T2]):-
    silence_program(Plans,Plans2),
    silence_program(T,T2).

silence_program([H|T], [H|T2]):-
    silence_program(T,T2).



silence_plan([],[]).

silence_plan([act(A is B)| Acts1], [act(A is B)| Acts2]):-
    silence_plan(Acts1, Acts2).

silence_plan([act(Act)| Acts1], [act(Act)| Acts2]):-
    Act=..[Operation, _, _],
    relational_operator(Operation),        % in the FragPLActions.pl file
    silence_plan(Acts1, Acts2).

silence_plan([act(Act)| Acts1], [(act(silently_(Act)))| Acts2]):-
%  the only modification is here (act that is not 'is' or relops)
    silence_plan(Acts1, Acts2).

silence_plan([act(Environment, Act)| Acts1],
             [(act(Environment, silently_(Act)))| Acts2]):-
%  the only modification is here (act that is not 'is' or relops)
    silence_plan(Acts1, Acts2).

silence_plan([H|T],[H|T2]):-
    silence_plan(T,T2).



%===============================================================================
%                                                                              |
%    MCTS Engine - one expansion, rollouts, backup                             |
%                                                                              |
%===============================================================================


%!  rewards_achieved(Rewards):-
                        
rewards_achieved(Rewards_Sum):-
    bagof(r(Loop, Reward), fact(reward(Loop, Reward)), Rewards),
    format(atom(RewardS),"Rewards in this run: ~w",[Rewards]),
    println_debug(RewardS, mctsdbg),    
    mcts_gamma(Discount),                          
    sum_rewards_list(Discount, Rewards, Rewards_Sum), 
    format(atom(RewardSS),"Rewards sum (with discounts): ~w",[Rewards_Sum]),
    println_debug(RewardSS, mctsdbg).
    	

rewards_achieved(0):-
    format(atom(RewardS), "No rewards in this run", []),
    println_debug(RewardS, mctsdbg).


sum_rewards_list(_, [], 0).

sum_rewards_list(Discount, [Reward| Rewards], Rewards_Sum):-
    compute_reward(Discount, Reward, Reward_Value),
    sum_rewards_list(Discount, Rewards, Reward_Sum2),
    Rewards_Sum is Reward_Sum2 + Reward_Value.

% Reward*Discount^Loop
compute_reward(Discount, r(Loop, Reward), Reward_Value):-
    Loop>0,
    Gamma is Discount ** Loop,
    Reward_Value is Reward * Gamma.

compute_reward( _, _, 0).



%    Simulations - rollouts
%==============================================================================


mcts_rollouts(Program, Steps, 0, Results, Rewards_Path, Expanded):-
    number_of_top_level_goals(Goals_Remain),
    format(atom(ResultsS), "~w", 
                [runResult(Results, 0, Expanded, Goals_Remain)]),
    println_debug(ResultsS, mctsdbg),
    thread_self(Virtual_Agent),
    remove_all_instances_state(Virtual_Agent, mcts_save),
    remove_clones(Virtual_Agent),
    close_engine_file,				% close stream
    engine_yield(runResult(results(Rewards_Path, Results), 0, Expanded, 
		 Goals_Remain)),
    !,
    garbage_all,
    
	% ENGINE RESTARTS HERE!

    garbage_collect_atoms,
    mcts_number_of_simulations(Simulations),
    mcts_rollouts(Program, Steps, Simulations, [], Rewards_Path, 
			  Expanded).



mcts_rollouts(Program, Steps, Simulations, Results, Rewards_Path, 
	              Expanded):-
    retractall(loop_number( _ )), % init run
    assert(loop_number(1)),
    clear_agent,
    !,
    thread_self(Agent),
    reset_clones(Agent),
    set_clauses(Program, 1),
    thread_self(Virtual_Agent),
    load_all_instances_state(Virtual_Agent, mcts_save),
    format(atom(ProgramS), "Program to simulate: ~w", [Program]),
    println_debug(ProgramS, mctsdbg),
    loop(Steps, Steps_Left),
    rewards_achieved(Rewards), 
    Steps_Done is Steps - Steps_Left,
    delete_clauses,
    !,
    garbage_all,
    Simulations2 is Simulations - 1,
    mcts_rollouts(Program, Steps, Simulations2, [Rewards| Results],
                          Rewards_Path, Expanded).



garbage_all:-
    garbage_collect,
    garbage_collect_atoms,
    garbage_collect_clauses,
    trim_stacks.


open_file(Filename):-
    append(Filename).

open_file(Filenam):-
    tell(Filename).


open_engine_file(Agent, Agent_Loop, 0):-
    agent_debug(mctsdbg),
  %  current_module(fRAg, FRAg_Path),
  %  file_directory_name(FRAg_Path, Directory),
    format(atom(DirectoryS), "logs/~w", [Agent]),
    try_make_directory(DirectoryS),
    format(atom(Filename),"~w/__mcts_engine_stp~w.mcts", [DirectoryS, 	
							Agent_Loop]),
    open_file(Filename),
 writeln(nazddaaar).

open_engine_file(Agent, Agent_Loop, Runs):-
    agent_debug(mctsdbg),
 %   current_module(fRAg, FRAg_Path),
 %   file_directory_name(FRAg_Path, Directory),

%    format(atom(DirectoryS), "~w/logs/~w/~w", [Directory, Agent, Agent_Loop]),
    format(atom(DirectoryS), "logs/~w", [Agent]),
    try_make_directory(DirectoryS),
    format(atom(Filename),"~w/_mcts_engine_stp~w-exp~w.mcts", [DirectoryS,
						             Agent_Loop, Runs]),
    tell(Filename).

open_engine_file( _, _, _).



try_make_directory(Directory):-
    exists_directory(Directory).

try_make_directory(Directory):-
    make_directory(Directory).



close_engine_file:-
    agent_debug(mctsdbg),
    told.

close_engine_file.



set_debugs(true):-
    assert(agent_debug(mctsdbg)),
    assert(agent_debug(reasoningdbg)).

set_debugs(false).


%!  mcts_frag_engine(+Program, +Inetntion_Fresh, +Event_Fresh, ?Expanded,
%                    +Runs, +Steps, +Simulations, +Agent_Loop, +Agent, 
%                    +Bindings, +Debug) is nondet
%   @ This engine performs a one-level expansion from the current node and 
%   a simulation from it.
%  @arg Program: program to be executed in the simulations  
%  @arg Intention_Fresh: fresh intention identifier, is the same as that of the
%                        original agent, which doesn't matter because the 
%			 agent overwrites these possibly used identifiers
%  @arg Event_Fresh: as in the previous case, but for events
%  @arg Expanded: nodes to be expanded in this step - actions and chosen plans


mcts_frag_engine(Program, Intention_Fresh, Event_Fresh, Path, Expanded,
		 Runs, Steps, Simulations, Agent_Loop, Agent, Bindings, Debug):-

    %
    %  simulation engine ...
    %  1, performs program 'Path'
    %  2, makes expansion at this point (for the leaf node of Program)
    %    ?? result of expansions is in Expanded (in which form?)
    %  3, makes simulation of postfix of the program (Program2)
    %      at this point as an engine - each execution of engine makes number_of_simulations(NOS) simulations
    %
    set_prolog_stack(trail, limit(5 000 000 000)),
    set_prolog_stack(local, limit(5 000 000 000)),

    assert(virtual_mode(true)),
    set_debugs(Debug),
    open_engine_file(Agent, Agent_Loop, Runs),
    set_late_bindings(Bindings),
    set_reasoning(random_reasoning),
    assert(loop_number(-1)),
    set_clauses(Program, 1),
    thread_self(Virtual_Agent),
    virtualize_agent(Agent, Virtual_Agent),
% saves fresh IDs for intention and event
    assert(intention_fresh(Intention_Fresh)),		
    assert(event_fresh(Event_Fresh)),
    println_debug('+++++ PATH +++++', mctsdbg),
    mcts_print_path(Path, mctsdbg),
    print_state('VVVVVVVVVV MCTS BEFORE FORCE PATH VVVVVVVVVV'),
% executes program in Path
    force_execute_model_path(Path, Rewards_Path),			
    print_state('VVVVVVVVVV MCTS AFTER FORCE VVVVVVVVVV'),
    model_expand_actions(Expanded_Acts),
    model_expand_deliberations(Expanded_Plans),
    append(Expanded_Acts, Expanded_Plans, Expanded),
    format(atom(ExpandedS), "Expanded nodes: ~w ~n Rewards on path ~w~n", 
	   [Expanded, Rewards_Path]),
    println_debug(ExpandedS, mctsdbg),

    take_snapshot(Program2),
    save_all_instances_state(Virtual_Agent, mcts_save),
    !,
    mcts_rollouts(Program2, Steps, Simulations, [], Rewards_Path, 
			  Expanded).   % Why Expanded? TODO


%! force_execute_model_path ... druhy term rewardy pri exekuci

force_execute_model_path([_, success], [0]).

force_execute_model_path([], []).

force_execute_model_path([_,  model_act_node( _, no_action, _) | Nodes], 
			 Rewards_Path):-
    force_execute_model_path(Nodes, Rewards_Path).

force_execute_model_path([node(Node_ID),  
			  model_act_node(Intention, Act, Context)
                            | Nodes], [Reward | Rewards_Path]):-
    % in FRAgAgent.pl
    force_execution(Node_ID, model_act_node(Intention, Act, Context), Reward),
    force_perceiving,
    force_execute_model_path(Nodes, Rewards_Path).

force_execute_model_path([node(Node_ID),  
			  model_reasoning_node(Goal, Plan_Number, Context)
                            | Nodes], [0 | Rewards_Path]):-
    force_reasoning(Node_ID, model_reasoning_node(Goal, Plan_Number, Context)),
    force_execute_model_path(Nodes, Rewards_Path).  

force_execute_model_path([leaf_node(Node_ID), 
                          model_act_node(Intention, Act, Context)],
			 [Reward]):-
    force_execution(Node_ID, model_act_node(Intention, Act, Context), Reward),
    force_perceiving.

force_execute_model_path([leaf_node(Node_ID),  
                          model_reasoning_node(Goal, Plan_Number, Context)], 
			 [0]):-
    force_reasoning(Node_ID, model_reasoning_node(Goal, Plan_Number, Context)).



%===============================================================================
%                                                                              |
%    MCTS Algorithm - non-thread (performing steps 3.4) part here              |
%                                                                              |
%===============================================================================


%    The Goal-Plan tree construction starts here
%==============================================================================



%!  mcts_simulation(+Program, +Expansions, +Simultaions) is ndet
%   Simulates Program and creates goal-plan tree. Basically, it just
%   initializes the model and then executes mcts_expansion_loop, which does the
%   rest.
%  @arg Program: better actual agent configuration <PB, BB, IS, EQ>
%  @arg Expansions: number of expansion, = nodes of resulting GPT
%  @arg Simulations: number of random runs for each expansion


mcts_simulation(Program, Expansions, Simulations):-
    mcts_model_init,
    number_of_top_level_goals(Goals_Total),
    mcts_expansion_loop(Program, Expansions, Goals_Total, Simulations).



%!  mcts_expansion_loop(+Program, +Expansions, +Max_Reward,
%                       +Simulations) is det
%   Creates a goal - plan tree, or expands when there is already some, using
%   the MCTS method. For a given FRAg, the program performs the specified
%   number of expansions - this is the 'budget' for one update of this decision
%   model, and performs the specified number of simulations for each expansion.
%   The maximum reward, fe. the number of goals, is also needed to calculate 
%   the rewards.
%  @arg Program: better actual agent configuration <PB, BB, IS, EQ>
%  @arg Expansions: number of expansion, = nodes of resulting GPT
%  @arg Max_Reward: real or estimated maximum reward an agnt can reach
%  @arg Simulations: number of random runs for each expansion

mcts_expansion_loop( _, 0, _, _).
 % no expansions left


mcts_expansion_loop(Program, Expansions, Max_Reward, Simulations):-
    late_bindings(Bindings),
    % in FragMCTSModel.pl, second term is UCB (true) just score (false)

% Phase 1, Selection
    mcts_get_best_ucb_path(Path, true), 
    intention_fresh(Intention_Fresh),
    event_fresh(Event_Fresh),
    mcts_simulation_steps(Simulation_Steps),
    loop_number(Agent_Loop),
    thread_self(Agent),
    open_engine_file(Agent, Agent_Loop, 0),
    format(atom(PathS), "Path is ~w",[Path]),
    println_debug(PathS, mctsdbg),
    is_debug(mctsdbg, Debug),
    engine_create(run_result(_ ,_),
		  mcts_frag_engine(Program, Intention_Fresh, Event_Fresh, Path,
                                   Expanded, Expansions, Simulation_Steps,
                                   Simulations, Agent_Loop, Agent, Bindings,
                                   Debug),
                  Engine),


    format(atom(ProgramS), 'Engine program: ~w', [Program]),
    println_debug(ProgramS, mctsdbg),
% Phase 2+3, Expansion and Roll-outs

    engine_next(Engine, runResult(results(Rewards_Path, Results), 0, Expanded,
	        Goals_Remain)),


    println_debug('Engine finished', mctsdbg),
    engine_destroy(Engine),
    member(leaf_node(Leaf), Path),
    sumlist(Results, Sumlist),
    length(Results, Length),
    Reward is Sumlist/Length,
    format(atom(ResultsS), 'Rewards are: ~w::~w', [Rewards_Path, Reward]),
    println_debug(ResultsS, mctsdbg),
    format(atom(ExpandedS), 'Expanded ~w', [Expanded]),
    println_debug(ExpandedS, mctsdbg),
    format(atom(RewardS), 'Reward: ~w', [Reward]),
    println_debug(RewardS, mctsdbg),

    mcts_print_model(mctsdbg),
% Phase 4, Backpropagation

    reverse_tl(Path, PathR),
    reverse(Rewards_Path, Rewards_PathR),
    mcts_propagate_results(PathR, Rewards_PathR, Reward), 
    mcts_expand_node(Leaf, Expanded), 
    Expansions2 is Expansions - 1,
    format(atom(ExpansionsS),"Expansions: ~w", [Expansions2]),
    println_debug(ExpansionsS,  mctsdbg),
    close_engine_file,	
    !,
    mcts_expansion_loop(Program, Expansions2, Max_Reward, Simulations).
                                                                                    

%! reverse_twol(+List, -List) is det
%  olny top level elements pairs reversion

reverse_tl([], []).

reverse_tl([H, H2| T], L):-
    reverse_tl(T, L2),
    append(L2, [H, H2], L).

reverse([], []).

reverse([H | T], L):-
    reverse(T, L2),
    append(L2, [H], L).



%!  number_of_top_level_goals(-Number) is det
%   Computes sum of active intentions and active events
%   probably should be considered only unserviced achievement goals 

number_of_top_level_goals(Number):-
    % actually, top level events + number of active intentions
    number_of_top_level_events(Number1),
    number_of_intentions(Number2),
    Number is Number1 + Number2.

number_of_top_level_goals(0).


number_of_intentions(Number):-
    bagof(IntentionID, intention(IntentionID, _, _), Intentions),
    length(Intentions, Number).

number_of_intentions(0).


number_of_top_level_events(Number):-
    bagof(top_level_goal(Event_Type, Event_Atom, Context, History),
          event(_ , Event_Type, Event_Atom, null, Context,
                                                   active, History),
          Top_Level_Goals),
    length(Top_Level_Goals, Number).

number_of_top_level_events(0).




% TODO, just provisory reward computation based on number of Top-Level-Goals

/*
mcts_compute_reward(_ , 0, _, 0).

mcts_compute_reward(_ , _, 0, 0).

mcts_compute_reward(0, Goals_Total, Goals_Achieved, Reward):-
    Reward is (Goals_Achieved/Goals_Total).

mcts_compute_reward(Awerage, Goals_Total, Goals_Achieved, Reward):-
    Reward is ((1/(Awerage/3+1))*(Goals_Achieved/Goals_Total)),
    print_debug('R:', mctsdbg),
    println_debug(Reward, mctsdbg).
*/


%    Clauses exported for agent control loop (reasoning method interface)
%===============================================================================



%!  update_model(mcts_reasoning) is nondet
%   updates the model for additional clauses. In this version, it creates a new
%   goal plan tree and assigns MCTS-based ratings to each node. It then extracts
%   the best path from this tree

update_model(mcts_reasoning):-
    late_bindings(Bindings),             
    retractall(simulate_late_bindings( _ )),
    assert(simulate_late_bindings(Bindings)),         
% here allways late bindings (for mcts simulation)
% set_late_bindings(true),
% in Program is now a snapshot of actual agent state
    take_snapshot(Program),                              
% ProgramS is silent - does not act toward environments (neither 'prints')
    silence_program(Program, Silent_Program),                      
% Expansions <- number of expansions per (the next) simulation
    mcts_expansions(Expansions),                        
% Number of simulations per expansion
    mcts_number_of_simulations(Simulations),      

% SIMULATIONS (MCTS ALGORITHM)STARTS HERE
    mcts_simulation(Silent_Program, Expansions, Simulations),

    print_mcts_model(mctsdbg_path),            
    mcts_get_best_ucb_path(Path, false),     
% REASONING: 'reasoning node' prefix of PATH, ACT is the first ACT in PATH
    mcts_divide_path(Path, Reasoning, Act),


    format(atom(ReportS),"Reasoning prefix:~w
[MCTS] First action:~w",[Reasoning, Act]),
    println_debug(ReportS, mctsdbg),
    retractall(recomended_path( _, _)),
    assert(recomended_path(Reasoning, Act)).


print_mcts_model(Debug):-
    println_debug('MODEL', Debug),
    mcts_print_model(Debug),
    println_debug('', Debug).
%    println_debug('', Debug),
%    println_debug('', Debug),
%    in FragMCTSModel.pl, second term is UCB (true) just score (false)
%    mcts_get_best_ucb_path(Path, false),
%    println_debug('Best path is:', Debug),
%    mcts_print_path(Path, Debug).



% first act is the fourth in the path, the first tuple is root, the third is node id
get_first_act([_, _, ID, Act|_], ID, Act).

get_first_act(_, null, no_action).   % root only, finished


get_plan_for_goal(event(Event_ID, Event_Type, Event_Atom, INTENTION,
                        GOALCTX, STATE, HISTORY),
                  REASONINGNODES, Plan_ID):-

% plan(Plan_ID,PLANTYPE,PLANPREDICATE,CTXCONDITIONS,BODY,PLANCTX)):-

    member(model_reasoning_node(event(Event_ID, Event_Type, Event_Atom, INTENTION, GOALCTX, STATE, HISTORY),
    plan(Plan_ID, PLANTYPE,PLANPREDICATE,CTXCONDITIONS,BODY),PLANCTX),REASONINGNODES),
    delete(REASONINGNODES,
           model_reasoning_node(event(Event_ID, Event_Type, Event_Atom,
                                      INTENTION, GOALCTX, STATE, HISTORY),
    plan(Plan_ID,PLANTYPE,PLANPREDICATE,CTXCONDITIONS,BODY),PLANCTX), REASONINGNODES2),
    recomended_path( _, ACT),
    retractall(recomended_path(_,ACT)),                             % delete this reasoning node and update
    assert(recomended_path(REASONINGNODES2,ACT)).


get_plan_for_goal(_, _, no_plan, _).



%!  get_intention(mcts_reasoning, +Intentions, -Intention) is det
%
%  @arg Intentions: list of actual agent's Intentions, all active and blocked
%  @arg Intention: selected one of the actione intentions from Intentions
%    intention(Intention_ID, Content, active)

get_intention(mcts_reasoning, Intentions,
              intention(Intention_ID, Content, active)):-
    recomended_path(_ ,[model_act_node(Intention_ID,_,_)]),
    format(atom(IntentionS), '[GET INTENTION] ~w', 
           [model_act_node(Intention_ID, _, _)]),
    println_debug(IntentionS, interdbg),
    member(intention(Intention_ID, Content, active),Intentions).



rename_substitution_vars([],[],[]).

rename_substitution_vars([ _=C | T1], [B | T2], [B=C | T3]):-
    rename_substitution_vars(T1,T2,T3).



get_model_act(Model_Act, Substitution):-
%	recomended_path( _, [model_act_node( _, act(ENVIRONMENT, silently_(MODELACT)), [MODELSUBSTITUTION])]).
    recomended_path( _, [model_act_node( _, act(_ , silently_(Model_Act)), [Substitution])]).

get_model_act(Model_Act, Substitution):-
    recomended_path( _, [model_act_node( _, act(silently_(Model_Act)), [Substitution])]).

get_model_act(Model_Act, Substitution):-
    recomended_path( _, [model_act_node( _, test(Model_Act), [Substitution])]).
                        
get_model_act(Model_Act, Substitution):-
    recomended_path( _, [model_act_node( _, add(Model_Act), [Substitution])]).

get_model_act(Model_Act, Substitution):-
    recomended_path( _, [model_act_node( _, del(Model_Act), [Substitution])]).



%!  get_substitution(random_reasoning, +Action,
%                    +Context, +Vars, -Context_Out) is det
%   CAN decisioning in early bindings mode selects substitution randomly.
%   This clause is to select one of the set of substitutions and reduces it to
%   just the variables from Vars.
%  @arv Action: action the agent is about to execute
%  @arg Context_In: actual context of the agent
%  @arg Vars: the variables for which a decision is to be made
%  @arg Context_Out: output context for the Action


get_substitution(mcts_reasoning, Action, Contexts, Vars, Context_Out):-
% findings_deliberation_made( First level of model ... list of [deliberation(goal,plan)]

    println_debug('[GET SUBSTITUTION]', interdbg),
    format(atom(ActionS), "GS1 Action: ~w", [Action]),
    println_debug(ActionS, interdbg),

    format(atom(ContextsS), "GS2 Context: ~w", [Contexts]),
    println_debug(ContextsS, interdbg),   % nepotrebujeme kontext, mame ho v modelu
    format(atom(VarsS), "GS3 Variables: ~w", [Vars]),
    println_debug(VarsS, interdbg),

    get_model_act(Model_Act, Model_Substitution),

% recomended_path(REASONING,[model_act_node(_,_,[MODELSUBSTITUTION])]),

    format(atom(Model_ActS), "GS4 Model Act: ~w", [Model_Act]),
    println_debug(Model_ActS, interdbg),
    format(atom(Model_SubstitutionS), "GS5 Model Context: ~w", 
                [Model_Substitution]),
    println_debug(Model_SubstitutionS, interdbg),
    apply_substitutions(Model_Substitution),
    format(atom(Model_SubsS), "GS6 Model Act Subs: ~w", [Model_Act]),
    println_debug(Model_SubsS, interdbg),
    %	rename_substitution_vars(MODELCTX,VARS,NCTX),
    unifiable_s(Action, Model_Act, Context_Out),
    format(atom(Context_OutS), 'GS7 Context Out: ~w', [Context_Out]),
    println_debug(Context_OutS, interdbg).


unifiable_s(Act1, Act2, Context):-
    get_act_content(Act1, Act_Content1),
    get_act_content(Act2, Act_Content2),
    unifiable(Act_Content1, Act_Content2, Context).	


get_act_content(silently_(Content), Content).

get_act_content(Content, silently_(Content)).
               
get_act_content(not(Content), Content).

get_act_content(Content, not(Content)).

get_act_content(Act, Act).


%!  get_plan(mcts_reasoning, +Event, +Means, -Intended_Means) is det
%   Depending on how the model (tree) created in the simulations looks like, it
%   selects the means for the event from the listed means according to
%   the best evaluation.
%  @arg Event:
%  @arg Means:
%  @arg Intended_Means:


get_plan(mcts_reasoning, Event, Means, [IM_Plan, IM_Context]):-
    format(atom(PlanS),"[GET PLAN]: Event: ~w~nPossible Means: ~w",
		        [Event, Means]),
    println_debug(PlanS, interdbg),
    !,
    recomended_path([model_reasoning_node(Event, IM_Plan, IM_Context)| _], _),
    format(atom(NodesS), 'Recomended Plan ~w/~w:', [IM_Plan, IM_Context]),
    println_debug(NodesS, interdbg).

get_plan(mcts_reasoning, _, _, []).





%!  init_reasoning(mcts_reasoning) is det
%   initializes reasoning according to parameters, sets whether the agent works
%   in late or early bindings, the number of expansions and simulations

init_reasoning(mcts_reasoning):-
    % tohle lze vytahnout z agentniho lb pred spustenim mcts vlakna
    late_bindings(Bindings),
    % dtto
    assert(simulate_late_bindings(Bindings)),
    mcts_default_expansions(Expansions),
    mcts_default_number_of_simulations(Simulations),
    retractall(mcts_expansions( _ )),
    retractall(mcts_number_of_simulations( _ )),
    assert(mcts_expansions(Expansions)),
    assert(mcts_number_of_simulations(Simulations)),
    mcts_model_init.




