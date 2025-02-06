

:-module(fRAgAgent,
    [
	fa_init_agent / 2,
	include_reasoning_method /1,
	load_environment /1,
        set_control /1,
	set_default_reasoning /1,
	get_default_reasoning /3,
	set_plan_selection /1,
	set_default_plan_selection /1,
	set_intention_selection /1,
	set_default_intention_selection /1,
	set_substitution_selection /1,
	set_default_substitution_selection /1,
	set_default_environment /1,
	get_default_environments /1,
	set_reasoning_params /1,
	set_late_bindings /1,
	set_late_bindings /0,
	set_default_late_bindings /1,
	set_early_bindings /0,
	is_late_bindings /0,
	is_default_late_bindings /0,
	force_reasoning /2,
	force_execution /3,
	force_perceiving /0,
	take_snapshot /1
    ]
  ).


/**
<module> fRAgAgent

This module contains code for thread clauses declarations for individual agents

@author Frantisek Zboril
@version 0.95 (2021 - 2024)
@license GPL
*/


:- discontiguous init_reasoning/1.
:- discontiguous reasoning_method/1.
:- discontiguous get_substitution/5.
:- discontiguous get_plan/4.
:- discontiguous get_intention/3.
:- discontiguous execute/4.
:- discontiguous apply_substitutions/1.
:- discontiguous extend_intention/3.

%   Following clauses are defined in particular reasoning methods files.
%  @see reasoning_methods.pdf (TODO)

:- multifile init_reasoning /1.
:- multifile reasoning_method /1.
:- multifile get_intention /3.
:- multifile get_substitution /5.
:- multifile get_plan /4.
:- multifile update_model /1.

%   FRAg specific ops / late bindings etc.
:-include('FRAgPLFRAg.pl').

%   FRAg operations for relations and assignments
:-include('FRAgPLRelations.pl').

%   print, debug info, etc.
:-include('FRAgTerminal.pl').

:- use_module(library(thread)).

%   shared data among threads (agents etc.)
:-use_module('FRAgSync').

%   interface to environments
:-use_module('FRAgAgentInterface').
  



timeout(200).

%   no_job, because the 'init' agent should finish ASAP
%   @see documentation for termination modes

terminate(no_job).


%   dynamic atoms

:-dynamic default_late_bindings / 1.
:-dynamic default_environment /1.
:-dynamic terminate /1.
:-dynamic timeout /1.
:-dynamic start_time /1.
:-dynamic finish_time /1.


%!  plan(+Event_Type, +Event_Atom, +Conditions, +Context, +Body).

:-thread_local plan/5.


%!  fact(Belief_Atom).

:-thread_local fact/1.


%!  intention(+Intention_ID, +Plan_Stack, +Status).

:-thread_local intention /3.


%!  goal(+Type, +Atom, +Context).
%  @arg Type: goal type [only 'ach' in this version]
%  @arg Atom: goal atom
%  @arg Context: should be empty [[]], but can contain some substutitutions
%   in the form [[

:-thread_local goal /3.


%!  virtual_mode(-Virtual_Mode).
%   virtual mode is used in some reasoning method that use look ahead planning
%  @arg Virtual_mode: true or false

:-thread_local virtual_mode /1.


%!  event(+Event_ID, +Type, +Atom, +Parent_Intention, +Context, +Status,
%         +History).
%  @arg Event_ID: event identifier
%  @arg Type: 'ach' for achievement, 'add' for add, 'del' for delete
%  @arg Atom: event atom
%  @arg Parent_Intention: Intention that raised this event
%  @arg Context: event context - PUS, usualy empty, but it could be
%  @arg Status: event state - active / intention number,
%  @arg History: a list of details of plans that have already been tried for
%    the event, concretely list of 
%                    'used_plan(Plan_ID, Goal_Atom, Conditions, Context)' 
	           
:-thread_local event /7.



%!  intention_fresh(-Number)
%  @arg Number: Next possible Identifier number for agent's intention

:-thread_local intention_fresh / 1.

%!  event_fresh(-Number)
%  @arg Next possible ID Number for agent's event

:-thread_local event_fresh / 1.

:-thread_local loop_number /1.

:-thread_local agent_debug /1.

:-thread_local late_bindings /1.



include_reasoning_method(Filename):-
    load_files([Filename], [silent(true)]).



%!  set_reasoning_params(+Parameters) is det
%  @arg Parameters: reasoning_method(Parameters), where reasoning_method
%   is name of a supported reasoning method, @see reasonings.txt

set_reasoning_params(Parameters):-
% 'set_reasoning_method_params' clauses are defined in particular
% reasoning files
    set_reasoning_method_params(Parameters).



%!  load_environment(+Filename) is det
%   includes file library for environment
%  @arg Filename: environment source file

load_environment(Filename):-
    fRAgAgentInterface:add_environment_library(Filename).



%!  get_default_environment(-Environments) is det
%   provides all loaded Environments in the system (through FRAg
%   environment interface)
%  @arg Filename: active environment names

get_default_environments(Environments):-
    fRAgAgentInterface:get_all_environments(Environments).




write_stats(String):-
    open('stats.pl', append, Stats_File),
    thread_self(Agent),
    write(Stats_File, String),
    writeln(Stats_File,'.'),
    agents_stats(Stats_File),
    close(Stats_File).



% takes _stats /1 from BB and writes it out

agents_stats(Stats_File):-
   clause(env_utils:fact( _, _, _), _),
   thread_self(Agent),
   findall(stats(Agent, Environment, Instance, Stats),
	   env_utils:fact(Environment, Instance, stats_(Agent, Stats)),
			  Stats_List),
   write(Stats_File, Stats_List),
   writeln(Stats_File,'.').

agents_stats( _ ).


%===============================================================================
%                                                                              |
%    Beliefs processing                                                        |
%                                                                              |
%===============================================================================


%!  process_add_list(+Beliefs) is det
%   Incorporates Beliefs to agent's BB
%  @arg Beliefs: List of belief atoms

process_add_list([]).

process_add_list([Belief| Beliefs]):-
    fact(Belief),  % is already in BB
    process_add_list(Beliefs).

process_add_list([Belief| Beliefs]):-
    assert(fact(Belief)),
    create_event(add, Belief),
    process_add_list(Beliefs).



%!  process_delete_list(+Beliefs)
%   Removes Beliefs to agent's BB
%  @arg Beliefs: List of belief atoms

process_delete_list([]).

process_delete_list([Belief | Beliefs]):-
%    fact(Belief), % is in BB, should be deletd
    retractall(fact(Belief)),
    create_event(del, Belief),
    process_delete_list(Beliefs).

%   not present in BB, need not to be deleted
process_delete_list([_ |Beliefs]):-
    process_delete_list(Beliefs).



%===============================================================================
%                                                                              |
%    COMMUNICATION HANDLER                                                     |
%                                                                              |
%===============================================================================


%!  process_messages
%   Receives messages (through thread) and creates corresponding add
%   expected form message(sender,perfomatie,pld(payload))
%   events

process_messages:-
    thread_peek_message(Message),
    thread_get_message(Message),
    get_fresh_event_number(Event_Index),
    assert(event(Event_Index, add, Message, null, [[]], active, [])).

process_messages.




%===============================================================================
%                                                                              |
%    SENSING		                                                       |
%                                                                              |
%===============================================================================


%!  sensing is det
%   Processes agent input from the environment, including messages.

sensing:-
    thread_self(Agent),
    agent_perceives(Agent, Add_List, Delete_List),
    % conflict should be resolved in 'agent_perceived'
    process_delete_list(Delete_List),
    process_add_list(Add_List),
    process_messages.



%===============================================================================
%                                                                              |
%    ONE ACT EXECUTION, 4th interpretation level / execution                   |
%                                                                              |
%===============================================================================



%!  execute(+Intention, +Plan_Before, - Plan_After) is det
%  @arg Plan_Before: plan plan(Event_Atom, Conditions, Context, Plan_Body)
%  @arg Plan_After: state of the plan after execution of its actual act,
%   it means the first act in Plan_Body.


%  Adding belief to agent's belief base

execute(_ , plan(Event_Type, Event_Atom, Conditions, Context,
                 [add(Belief)| Acts]),
		 plan(Event_Type, Event_Atom, Conditions, Context2, Acts),
        true)
    :-
%   'decisioning' clause is defined in FRAgPLFrag.pl
    decisioning(Belief, Context, Context2, true),
    assert(fact(Belief)),
    create_event(add, Belief).


%  Deleting belief from agent's belief base

execute(_ ,
        plan(Event_Type, Event_Atom, Conditions, Context, [del(Belief)| Acts]),
	plan(Event_Type, Event_Atom, Conditions, New_Context, Acts),
        true):-
%   'decisioning' clause is defined in FRAgPLFrag.pl
    decisioning(Belief, Context, New_Context, true),
    try_retract_belief(fact(Belief)),
    create_event(del, Belief).


% Making instance set. Makes instance set of Atom using actual Context of
% the plan. Resulting set of Atom instances unified with the Instance_Set
% atom (usually Instance_Set is a variable, then the set is bounded to it).

execute(_ ,plan(Event_Type, Event_Atom, Conditions, Context,
                [iset(Atom, Instance_Set)| Acts]),
	   plan(Event_Type, Event_Atom, Conditions, Context, Acts), true):-
    loop_number(Loop_Number),
    format(atom(String), "[ACTING (~w)] Making instance set ~w",
		          [Loop_Number, iset(Atom, Instance_Set)]),
    println_debug(String, actdbg),

%   'instance_set' clause is defined in FRAgPLFrag.pl
    instance_set(Atom, Context, Instance_Set).


%   Performing the test goal

execute( _, plan(Event_Type, Event_Atom, Conditions, Context, 
                 [test(Goal)| Acts]),
	    plan(Event_Type, Event_Atom, Conditions, Context_New, Acts),
                 Result):-
    loop_number(Loop_Number),
    format(atom(String), "[~w] Execiuting query ~w", [Loop_Number, 
						      test(Goal, Context)]),
    println_debug(String, actdbg),

%   'query' clause is defined in FRAgPLFrag.pl
    query(Goal, Context, Context2),
    late_bindings(Bindings),
    simulate_early_bindings(Goal, Context2, Context_New, Bindings, true),
    format(atom(String2), "[~w] Test goal RESULT: ~w", [Loop_Number, 
							Context2]),
    println_debug(String2, actdbg),

%   If the resulting context is nonempty, the test goal is successful. Otherwise,
%   it is unsuccessful.
    nonempty_context(Context_New, Result).


%   Top level goal (external event) declaration

execute(Intention_ID,
        plan(Event_Type, Goal_Atom, Conditions, Context, [tlg(Goal)| Acts]),
        plan(Event_Type, Goal_Atom, Conditions, Context, Acts),
        true)                                                            
    :-
    loop_number(Loop_Number),
    format(atom(String), "[~w] Introducting top level goal ~w",
		          [Loop_Number, tlg(Goal, Context)]),
    println_debug(String, actdbg),

 %   variables of the goal declared
    term_variables(Goal, Goal_Variables),

%   short context of the original level by variables in the goal declared
%   'shorting' clause is defined in FRAgPLFrag.pl
    shorting(Goal, Goal2, Context, Goal_Variables, Context_New,_),
    get_fresh_event_number(Event_ID),

%   create ach event with proper parent intention and context
    assert(event(Event_ID, ach, Goal2, null, Context_New, active, [])).





%   Performing the achievement goal

execute(Intention_ID,
        plan(Event_Type, Goal_Atom, Conditions, Context, [ach(Goal)| Acts]),
        plan(Event_Type, Goal_Atom, Conditions, Context, [ach(Goal)| Acts]),
        true)                                                            
    :-
    loop_number(Loop_Number),
    format(atom(String), "[~w] Introducting achievement goal ~w",
		          [Loop_Number, ach(Goal, Context)]),
    println_debug(String, actdbg),


    retract(intention(Intention_ID, Plan_Stack, active)),
%   this intention is blocked now
    assertz(intention(Intention_ID, Plan_Stack, blocked)),

%   variables of the goal declared
    term_variables(Goal, Goal_Variables),

%   short context of the original level by variables in the goal declared
%   'shorting' clause is defined in FRAgPLFrag.pl
    shorting(Goal, Goal2, Context, Goal_Variables, Context_New,_),
    get_fresh_event_number(Event_ID),

%   create ach event with proper parent intention and context
    assert(event(Event_ID, ach, Goal2, Intention_ID, Context_New, active, [])).


% Plan is empty - nothing to do now. It will later be removed from the
% intention at higher-level reasoning

execute(_ ,plan(Event_Type, Goal_Atom, Conditions, Context, []),
        plan(Event_Type, Goal_Atom, Conditions, Context, []), true).


%   execution of relation / assertion act. For instance alop(X<3) or alop
%   (X is Y+1). It does not do decisioning, but it works with the whole
%   context. Resulting context contains all that survived this relational
%   constraint. F.e. [[X=1],[X=5],[X=3],[X=2]] -> [[X=1],[X=2]] for
%   alop(X<3).

execute(_ ,
        plan(Event_Type, Goal_Atom, Conditions, Context_In, [rel(Term1 is Term2)|
                                                            Acts]),
        plan(Event_Type, Goal_Atom, Conditions, Context_Out, Acts), Result)
    :-
    alop(Term1 is Term2, Context_In, Context_Out, Result).


execute(_ ,plan(Event_Type, Goal_Atom, Conditions, Context,
                [rel(Relation)| Acts]),
	plan(Event_Type, Goal_Atom, Conditions, Context_Out, Acts), Result)
    :-
    functor(Relation, Operator, _),
    loop_number(Loop_Number),
    format(atom(String), "[~w] Execiuting relation check  ~w",
		          [Loop_Number, rel(Relation)]),
    println_debug(String, actdbg),



%   'is_relational_operator' clause is defined in FRAgPLRealtions.pl
    is_relational_operator(Operator),
%   'alop' clause is defined in FRAgPLRealtions.pl
    alop(Relation, Context, Context_Out, Result).


% Acting in specified environment

execute(_ ,plan(Event_Type, Event_Atom, Conditions, Context,
                [act(Environment, Action)| Acts]),
	plan(Event_Type, Event_Atom, Conditions, Context_Out, Acts), Restult)
    :-
%   'decisioning' clause is defined in FRAgPLFrag.pl
    decisioning(Action, Context, Context_Out, true),
    !,
    loop_number(Loop_Number),
    format(atom(String), "[~w] Execiuting in environment ~w act ~w",
		          [Loop_Number, Environment, Action]),
    println_debug(String, actdbg),
    execute_environment(Environment, Action, Restult).


execute(_ , plan(Event_Type, Event_Atom, Conditions, Context, [act(Action)|
                                                             Acts]),
            plan(Event_Type, Event_Atom, Conditions, Context_Out, Acts), Result)
    :-
%   'decisioning' clause is defined in FRAgPLFrag.pl
    decisioning(Action, Context, Context_Out, true),
    !,
    % execute action in 'basic' FRAg environment
    loop_number(Loop_Number),
    format(atom(String), "[~w] Execiuting in environment basic act ~w",
		    [Loop_Number, Action]),
    println_debug(String, actdbg),
    execute_environment(basic, Action, Result).

% Act execution failed. Plan remains the same, the Result is 'false'

execute(_ ,
        plan(Event_Type ,Goal_Atom, Conditions, Context, Acts),
        plan(Event_Type ,Goal_Atom, Conditions, Context, Acts),
        false):-
 	loop_number(Loop_Number),
	format(atom(String), "[~w] Plan execution FAILED", 
			      [Loop_Number]),
    	println_debug(String, actdbg).




%!  execute_plan(+Intention_ID, +Plan_In, -Plan_Out, -Result) is nondet
%   Plan is linear and only one act is executed per cycle
%  @arg Intention_ID: Intention identifier
%  @arg Plan_In: Input / original plan 
%  @arg Plan_Out: Output / modified plan
%  @arg Result: true / false / atom

execute_plan(Intention_ID,
             [plan(Plan_ID, Event_Type, Event_Atom, Conditions, Context,
                   Body)| Plans],
	     [plan(Plan_ID, Event_Type, Event_Atom, Conditions, Context2,
                   Body2)| Plans],
             Result)
    :-
    execute(Intention_ID,
            plan(Event_Type, Event_Atom, Conditions, Context, Body),
	    plan(Event_Type, Event_Atom, Conditions, Context2, Body2),
            Result).



%!  execute_environment(+Environment, +Act, -Result) is nondet
%   Tries to perform Act in an instance of Environment, where agent is situated
%  @arg Environment: Environment identifier
%  @arg Action: specifies which Act is to perform in Environment
%  @arg Result: Result of execution (true / false / atom)

execute_environment(Environment, Act, Result):-
    thread_self(Agent_Name),
    fRAgAgentInterface:agent_acts(Agent_Name, Environment, Act, Result).

execute_environment( _, _, false).



%!  create_event(+Event_Type, +Atom) is det
%   Creates top-level event of corresponding type and atom + empty context
%  @arg Event_Type: Event type (ach / add / del)
%  @arg Atom: Event atom

create_event(Event_Type, Atom):-
    get_fresh_event_number(Event_Index),
    assert(event(Event_Index, Event_Type, Atom, null, [[]], active, [])).



%!  get_fresh_event_number(+Event_ID) is multi
%   Provides next fresh ID for an agent's event
%  @arg Event_ID: Fresh event identifier

get_fresh_event_number(Event_ID):-
    event_fresh(Event_ID),
    retract(event_fresh(Event_ID)),
    Event_ID2 is Event_ID+1,
    assertz(event_fresh(Event_ID2)).



%!  try_retract_belief(+Belief) is nondet
%   Retracts Belief from the agent's Belief Base without failing when
%   there is no such Belief there.
%  @arg Belief: Belief to retract

try_retract_belief(Belief):-
    retract(Belief),
    create_event(del, Belief).

try_retract_belief( _).



%!  nonempty(+Context, -Is_Empty) is det
%   Check whether Context is an empty list
%  @arg Context: context of interest
%  @arg Is_Empty: true / false depending of emptiness of Context

nonempty_context([], false).

nonempty_context( _, true).



%===============================================================================
%                                                                              |
%     EXEXUTION AND INTENTION PROCESSING, 3rd interpretation level             |
%                                                                              |
%===============================================================================



%    SELECT INTENTION
%===============================================================================


%!  select_intention(-Intention) is multi
%   Selects one active intention due to active reasoning method (see
%   FRAgPLFRAg.pl and FRAg*Reasoning.pl files) @tbd @arg Intention: Chosen
%   Intention from active intentions
%  @arg Intention: Selected intention

select_intention(intention(Intention_ID_Out, Plan_Stack_Out, Status_Out)):-
    findall(intention(Intention_ID, Plan_Stack, Status),
            intention(Intention_ID, Plan_Stack, Status), Intentions),
% 'get_intention' clause is defined in FRAgPLFrag.pl
    get_intention(Intentions, intention(Intention_ID_Out, Plan_Stack_Out,
                                        Status_Out)).




%    EXTEND INTENTION
%===============================================================================

%!  extend_intention(+Intention_ID, +Plan, -Status) is multi
% means found for top level goal

extend_intention(null, [plan(Plan_ID, Event_Type, Event_Atom, Conditions, Body),
                        Context], Intention_ID)
    :-
	get_fresh_intention_number(Intention_ID),
	assertz(
	    intention(Intention_ID, [plan(Plan_ID, Event_Type, Event_Atom,
                                          Conditions, Context, Body)], active
                    )
	       ).


%   means found for a subgoal

extend_intention(Intention_ID, [plan(Plan_ID, Event_Type, Event_Atom, Conditions,
                                     Body), Context], Intention_ID)
    :-	retract(intention(Intention_ID, Plan_Stack, blocked)),
	assertz(
	   intention(Intention_ID,
		[plan(Plan_ID, Event_Type, Event_Atom, Conditions, Context, Body)
                | Plan_Stack],active
	            )
		).

%   exception / error raised when Indention_ID does not match any intention
%   (this clause should never be reached, but for sure)

extend_intention(Intention_ID, Plan_Stack, Status):-
    format(atom(String),"Lost intention ~w",
           [intention(Intention_ID, n, Plan_Stack, Status)]),
    println_debug(String, error).



%!  get_fresh_intention_number(+Intention_ID) is multi
%   Provides next fresh ID for an intention
%  @arg Intention_ID: Fresh intention ID

get_fresh_intention_number(Intention_ID):-
    intention_fresh(Intention_ID),
    retract(intention_fresh(Intention_ID)),
    Intention_ID2 is Intention_ID+1,
    assertz(intention_fresh(Intention_ID2)).




%    UPDATE EVENT
%===============================================================================


%!  update_event(+New_Intention_ID, +Event, +Result, +History) is det
%  @arg New_Intention_ID: ID of intention processing the event or -1 if none
%       was foundnot been found.
%  @arg New_Intention_ID: Identifier of intention assigned to the Event
%  @arg Event: Event in its usual structure
%  @arg Result: Result of reasoning about the Event ('active'/ 'false')
%  @arg History: List of plans tried for this Event

% Means for the top level ach goal found, active -> intention =blocked

update_event(Intention_ID_New, event(Event_ID, ach, Event_Atom, Intention_ID,
                                     Event_Context, active, History),
             true,
             [plan(Plan_ID, _, Goal_Atom, Conditions, _), Context])
    :-
    assert(event(Event_ID, ach, Event_Atom, Intention_ID, Event_Context,
                 Intention_ID_New,
		 [used_plan(Plan_ID, Goal_Atom, Conditions, Context)| History]
                )
          ).



% No means for an achieve goal, put the goal back

update_event( _, event(Event_ID, ach, Event_Atom, Parent_Intention, Context,
                       active, _), false, _)
    :-
% resets history
    assert(event(Event_ID, ach, Event_Atom, Parent_Intention, Context,
                 active, [])).


% Other event types (add/del) are removed in both cases, whether a resource has
% been found for them or not

update_event( _, event( _, _, _, _, _, active, _), _, _).


%!  try_retract_belief(+Intention_ID) is nondet
%   Retracts Belief from the agent's Belief Base without failing when
%   there is no such Belief there.
%  @arg Belief: Belief to retract

%!  try_retract_event(+Intention_ID) is nondet
%  @tbd

try_retract_event(Intention_ID):-
    findall(Event_ID, event(Event_ID, _, _, _, _, Intention_ID, _),
            Event_IDs),
    max_list(Event_IDs, Event_ID_Max),
    retract(event(Event_ID_Max, Type, Atom, Intention, Context, _, History)).
%    loop_number(Loop_Number),
%    assert(event(Event_ID_Max, Type, Atom, Intention, Context, 
%                  reached(Loop_Number), History)).

try_retract_event( _).



try_refresh_event(Intention_ID):-
    findall(Event_ID, event(Event_ID, _, _, _, _, Intention_ID, _),
            Event_IDs),
    max_list(Event_IDs, Event_ID_Max),
    retract(event(Event_ID_Max, Type, Atom, Intention_ID, Context, _,
                  History)),
    assert(event(Event_ID_Max, Type, Atom, Intention_ID, Context, active, 
                 History)).
   

try_refresh_event( _).



%    UPDATE INTENTION
%===============================================================================


%!  update_intention(+Intention, +Result) is det
%   Processing the outcome of an act at the level of intentions. There
%   may be a successful completion of a top-level plan or sub-plan (in a
%   given intention), a top-level plan or sub-plan fails by an
%   unsuccessfully executed act, or only a successfully executed act in a
%   plan that was not the last act. If the intent is blocked, no change
%   occurs.
%  @arg Intention: intention identifier
%  @arg Result: false, if previous ste failed, true when it succeed, or 
%   a number when it succeed and got reward
%  @tbd navazat na pravidla z clanku
%


%  Act succeeded and producet reward. Save reward info and do this agent 
%  with result 'true'

update_intention(Intention, reward(Reward)):-
    format(atom(String),
           'Update intention: REWARD OBTAINED ~w',[Reward]),
    println_debug(String, reasoningdbg),
    loop_number(Loop_Number),
    assert(fact(reward(Loop_Number, Reward))),
    update_intention(Intention, true).


% Intention is blocked, it means that an achievement goal for this
% intention has just been executed. It remains blocked until this goal
% is processed

update_intention(intention(Intention_ID, _, _), true):-
    intention(Intention_ID, _, blocked),
    format(atom(String),
           'Update intention: INTENTION BLOCKED',[]),
    println_debug(String, reasoningdbg).


% Top-level plan succeeded - there is only one plan in intention's plan
% stack and its body is empty. Removes the intention and the event that
% caused the intention. If the event was of type add/del, then it was
% forgotten earlier.

update_intention(intention(Intention_ID, [plan(_, _, _, _, _, [])], _),
                 true)
    :-
    format(atom(String),
           'Update intention: TOP LEVEL PLAN SUCCEEDED',[]),
    println_debug(String, reasoningdbg),
    retract(intention(Intention_ID, _, _)),
    try_retract_event(Intention_ID).


% Subplan succeeded - plan on top of the plan stack has empty body.
% Takes the context to a higher level. Dale removes the act of
% achievement goal from the higher level plan. Need to re-execute
% update_intention in case this act is the last act of the plan at the
% parent level.

update_intention(intention(Indention_ID,
                           [plan( _, _, Event_Atom, _, Context, []),
                            plan(Plan_ID2, Event_Type2, Event_Atom2,
                                  Conditions, Context2, [ach(Goal)| Acts])
                            | Plans],
                           Status),
                 _ )
    :-
    format(atom(String),
           'Update intention: SUBPLAN SUCCEEDED',[]),
    println_debug(String, reasoningdbg),
    intersection(Event_Atom, Context, Goal, Context2, Context3),
    retract(intention(Indention_ID, [ _, _| Plans], Status)),
    assertz(intention(Indention_ID, [plan(Plan_ID2, Event_Type2, Event_Atom2,
                                          Conditions, Context3, Acts)| Plans],
                      Status)),
    try_retract_event(Indention_ID),
    update_intention(intention(Indention_ID,
                               [plan(Plan_ID2, Event_Type2, Event_Atom2,
                                     Conditions, Context3, Acts)| Plans],
                                Status),
                      true).


% Top-level plan failed. Removes the intention and if it is possible to
% remove event (in the case that the event is 'ach' type), it will
% restore it.

update_intention(intention(Intention_ID, [ _ ], Status), false):-
    loop_number(Loop_Number),
    format(atom(String),
           '[~w] Update intention: SUBPLAN FAILED',[Loop_Number]),
    println_debug(String, reasoningdbg), 
%    format(atom(String2), "[~w] Update intention: ACTION FAILED", 
%                           [Loop_Number]),
%    println_debug(String2, actdbg),
    retract(intention(Intention_ID, _, Status)),
    retract(event(Event_Index, Type, Atom, null, Context, Intention_ID,
                  History)),
    assertz(event(Event_Index, Type, Atom, null, Context, active, History)).

% Top-level plan failed. The event was no longer in the system
% ('add'/ 'del' types), then it remains forgotten

update_intention(intention(_, [ _ ], _), false).

% Sub-plan failed - a failed action in a sub-plan, the plan is
% removed from the intention and the higher level plan is reactivated.
% Also corresponding event is removed (in this version only 'ach' event
% may cause sup-plans). Then in will be again raised when the higher
% level plan executes the achievement goal.

update_intention(intention(Intention_ID,
                           [plan( _, Event_Type, Event_Atom, _, _, _)| Plans],
                           Status), false):-
    loop_number(Loop_Number),
    format(atom(String), '[~w] Update intention: SUBPLAN FAILED', 
                          [Loop_Number]), 
    println_debug(String, reasoningdbg),
%   format(atom(String2), "[~w] Update intention: ACTION FAILED", 
%			   [Loop_Number]),
%    println_debug(String2, actdbg),
    retract(intention(Intention_ID, _, Status)),
    try_refresh_event(Intention_ID),
    assertz(intention(Intention_ID, Plans, blocked)).

/*    retract(event( _, Event_Type, Event_Atom, _, _, Intention_ID, _)),
    assertz(intention(Intention_ID, Plans, active)).
*/

%  The act has been successfully carried out and the plan continues. Its
%  current state is in the Plan_Stack variable. We change the intention
%  to its current state.

update_intention(intention(Intention_ID, Plan_Stack, Status), _):-
    loop_number(Loop_Number),
    format(atom(String), '[~w] Acting result: ACTION SUCCEEDED', 
			  [Loop_Number]),
    println_debug(String, actdbg),
    retract(intention(Intention_ID, _, Status)),
    assertz(intention(Intention_ID, Plan_Stack, Status)).



%!  update_intentions(-Result) is det
%   Update Intentions - remove joint action from the top of any
%   intention
%  @arg Result: Result of last performed act (true / false/ atom).

%   Result is an atom that can be found in one or more
%   intentions as the first act of a plan on the top of its stack in the
%   form act(Result). Then such acts are removed from all such
%   intentions.

update_intentions(Result):-
    intention(Intention_ID,
              [plan(Plan_ID, Event_Type, Goal_Atom, Conditions, Context,
                    [act(Result)| Acts])| Plans], active),
    short_variables_binds(Context, Context_New),
    update_intention(intention(Intention_ID,
                               [plan(Plan_ID, Event_Type, Goal_Atom,
                                     Conditions, Context_New, Acts)
                               | Plans],
                               active),
                     true),
    update_intentions(Result).

%  There is no more 'act(Result)' ready to be performed in any
%  intention.

update_intentions( _).



%===============================================================================
%                                                                              |
%    REASONING, 3rd interpretation level                                       |
%    (event selection, relevant and applicable plan / intended mean selection) |
%                                                                              |
%===============================================================================



%!  get_relevant_applicable_plans(+Event_Type, +Event_Atom, +Context,
%                                 -Means_Out) is det
%  @arg Event_Type: Type of event in WEI of interest
%  @Event_Atom: Event atom in WEI of interest
%  @Event_Context: Context in WEI of interest
%  @arg Means_Out: Possible means created from agent's relevant and
%   applicable plans

get_relevant_applicable_plans(Event_Type, Event_Atom, Event_Context, Means_Out)
    :-
    findall(plan(Plan_ID, Event_Type, Event_Atom, Context_Conditions, Body),
	  plan(Plan_ID, Event_Type, Event_Atom, Context_Conditions, Body),
          Means1),
    check_relevant_applicable_plans(Event_Atom, Event_Context, Means1, Means2),
    check_early_reasoning(Means2, Means_Out).

% this clause should not be reached (and should be deleted)

get_relevant_applicable_plans(_,_,[],[]).


%!  check_relevant_applicable_plans(+Event_Atom, +Event_Context,
%                                   +Plans_In, -Plans_Out) is det
%   Selects from +Plans_In those that are relevant to +Event_Atom and
%   applicable in Context. Such plans are then in -Plans_Out
%  @Event_Atom: Event atom in WEI of interest
%  @Event_Context: Context in WEI of interest
%  @arg Plans_In: List of available agent's Plans
%  @arg Plans_Out: Relavant and applicable agent's Plans


% no more plans

check_relevant_applicable_plans( _, _, [], []).

% The first Plan in the list is relevant and applicable, insert it to
% the Means_Out list of plans

check_relevant_applicable_plans(Event_Atom, Context, [Plan| Plans], Means_Out)
    :-
    % H pokud projde, bude v H2 jako [[H,Kontext]], jinak []
    check_relevant_applicable_plan(Event_Atom, Context, Plan, Means1),
    !,
    check_relevant_applicable_plans(Event_Atom, Context, Plans, Means2),
    append(Means1, Means2, Means_Out).

% The first Plan in the list is not relevant, forget it

check_relevant_applicable_plans(G, Context_Conditions, [_ | Plans], T2)
    :-
    check_relevant_applicable_plans(G, Context_Conditions, Plans, T2).


%!  check_relevant_appliable_plan(+Event_Atom, +Event_Context, +Plan,
%                                 -Means) is det
%   Provides Means if Plan in Context is relevant and applicable. New
%   context is computed (@see [1], Definition 11) and if it is not
%   empty, then new Means is provided as the Plan and the new context.
%  @Event_Atom: Event atom in WEI of interest
%  @Event_Context: Context in WEI of interest
%  @Plan: Plan of interest (using Goal instead of Event here to
%   distinguish between Event atom and Plan's event atom
%  @Means: Constructed means (tuple list constiting of the Plan and the
%   new Context) or []

check_relevant_applicable_plan(Event_Atom, Event_Context,
				plan(Plan_ID, Goal_Type, Goal_Atom,
				     Conditions, Body),
				Means):-
    intersection(Event_Atom, Event_Context, Goal_Atom, Context2),
    !,
    check_applicable(Context2, Conditions, Context3),
    is_means(Context3, [[plan(Plan_ID, Goal_Type, Goal_Atom,
			 Conditions, Body), Context3]],
	     Means).


%  Empty context -> no means, else accept the [Plan, New_Context] as a
%  means.

is_means([], _, []).

is_means( _, Means, Means).


%!  check_applicable(+Context_In, +Conditions, -Context_Out).
%   With given Context_In and a set of Conditions computes new Context.
%   If the Context_Out is an empty list, then the Conditions in the
%   Context_In are not valid in agent's belief base. @arg @arg @arg
%  @arg Context_In: initial context
%  @arg Conditions: set of conditions / guards, relations or queries
%  @arg Context_Out: resulting context (if [], then 'not applicable')

check_applicable(Context, [], Context).

% 'true' allways succeeds and does not change context

check_applicable(Context, [true| Conditions], Context_Out):-
    check_applicable(Context, Conditions , Context_Out).

% condition is a relation <  >  =  ==

check_applicable(Context, [Relation| T], Context_Out):-
    Relation=..[Operator, _, _],
    is_relational_operator(Operator),
    !,
% 'alop' clause is defined in FRAgPLRelations.pl
    alop(Relation, Context, Context2, true),
    check_applicable(Context2, T, Context_Out).

% condition is a query

check_applicable(Context, [Context_Condition| Context_Conditions],
                 Context_Out):-
% 'query' clause is defined in FRAgPLFrag.pl, here it is allways late
% then for every specification is returned one plan
    query(Context_Condition, Context, Context2, true),
    !,
    check_applicable(Context2, Context_Conditions, Context_Out).



%!  check_early_reasoning(+Means_In, -Means_Out).
%   If late bindings is not used, then cut contexes of top level (it should be
%   enough) plans of each intention to size max 1
%  @arg Means_In:
%  @arg Means_Out:

% For the case that there are no plans on input

check_early_reasoning([], []).

% Late bindings is default mode of reasoning mode, 7so there is no need
% to do anything.

check_early_reasoning(Means, Means):-
    late_bindings(true).

% No late bindings means early bindings. The means need to be specified
% by conctete instances.

check_early_reasoning(Means_In, Means_Out):-
    simulate_early_reasoning(Means_In, Means_Out).


%!  simulate_early_reasoning(+Means_In, -Means_Out) is det
%  @Means_In:
%  @Means_Out:

simulate_early_reasoning([], []).

simulate_early_reasoning([[plan(Plan_ID, Event_Type, Goal_Atom,
                                Conditions, Body), Context]| Plans],
                           Means)
    :-
    expand_plans([plan(Plan_ID, Event_Type, Goal_Atom, Conditions, Body),
                  Context], Means1),
    simulate_early_reasoning(Plans, Means2),
    append(Means1, Means2, Means).


%!  expand_plans(Plans1, Plans2) is det
%   Divides a single WPI with a context that can contain multiple
%   substitutions into a set of WPIs with contexts that contain only one
%   substitution
%  @Means_In: One means in the form of WPI [plan_definition, context]
%  @Means_Out: Set of means after crushing the Means_In into WPI
%   with only one substitution in context

% no more substitutions in context

expand_plans([plan( _, _, _, _, _), []]  , []).

% take the first substitution, create specific WPI and go in with the
% rest of substitutions

expand_plans([plan(Plan_ID, Type, Atom, Conditions, Body),
              [Substitution| Contexts]],
             [[plan(Plan_ID, Type, Atom, Conditions, Body),
               [Substitution]] | Plans]):-
    expand_plans([plan(Plan_ID, Type, Atom, Conditions, Body),
                  Contexts], Plans).



%===============================================================================
%                                                                              |
%    REASONING and act EXECUTION, 2nd interpretation level                     |
%                                                                              |
%===============================================================================


%!  execution is multi
%   Executes one Act. Intention is selected by actual intention selection
%   method, then the plan on the top of its plan stack is executed. Then
%   the plan is inserted back into the plan base at the end of it and
%   intention is appropriately updated due to the result. Then, for the
%   case of joint actions, rest of the intentions is updated as well

acting:-
    select_intention(intention(Intention_ID,
                               [plan(Plan_ID, Event_Type, Event_Atom, Conditions,
                                     Context, Body)| Plans],
                               Status)),
    !,
    execute_plan(Intention_ID,
                 [plan(Plan_ID,Event_Type, Event_Atom, Conditions, Context, Body)
                   | Plans],
                 Plan2, Result),
    put_back_plan(Plan_ID, Result),
    update_intention(intention(Intention_ID, Plan2,Status), Result),
% if Result is an atom, then a joint action was executed and it should
% be removed from all intention where it is the actual act for execution
    update_intentions(Result).

acting.  % no intention in agent's


%!  put_back_plan(Plan_ID, Status) is multi
%   If Staus is 'false' then plan with Plan_ID is asserted at the
%   end of database.
%  @arg Plan_ID: plan identifier 
%  @arg Status: false -> plan failed, put it in the back of plan base, else do 
%   nothing

put_back_plan(Plan_ID, false):-
    %   printfg("Giving the plan to the end of PB",[IDX]),
    retract(plan(Plan_ID, Event_Type, Event_Atom, Conditions, Body)),
    assertz(plan(Plan_ID, Event_Type, Event_Atom, Conditions, Body)).

put_back_plan(_,_).     % akce planu byla OK, nedavame nakonec


%!  deliberation is det
%   Processes all events simultaneously. For each event a means is searched.
%   If found, the corresponding intention is created or expanded

deliberation:-
    findall(event(Event_ID, Type, Atom, Parnt_Intention, Context, active,
                  History),
            event(Event_ID, Type, Atom, Parnt_Intention, Context, active,
                  History),
            Events),
    deliberation2(Events).

% no events to process

deliberation.


%!  deliberation2(+Events) is det
%   Process event by event from Events
%  @arg Events:

deliberation2([]).

deliberation2([Event | Events]):-
    retract(Event),
    deliberation3(Event),
    deliberation2(Events).


%!  reasoning3(+Event) is det
%   Tries to find a means for Event, if successful, expands or creates
%   relevant intention
%  @arg Event: event as 
%     event(+Event_ID, +Type, +Atom, +Parent_Intention, +Context, +Status,
%           +History)

deliberation3(event(Event_ID, Event_Type, Event_Atom, Parent_Intention,
                 Context, active, History)):-
    get_relevant_applicable_plans(Event_Type, Event_Atom, Context, Means),
    format(atom(String5), 'Possible means: ~w', [Means]),
    println_debug(String5, interdbg),
    
    deliberation4(Event_ID, Event_Type, Event_Atom, Parent_Intention,
                       Context, History, Means).

deliberation3(event(Event_ID, Event_Type, Event_Atom, Parent_Intention, Context,
                 active, History)):-
    update_event(-1, event(Event_ID, Event_Type, Event_Atom, Parent_Intention,
                           Context, active, History),
                 false, _).


%!  deliberation4(+Event_ID, +Event_Type, +Event_Atom, +Parent_Intention,
%              +Context +History, -Means) is nondet
%   For event structure (Event_ID .. History) in the case that some
%   means are proposed (from reasoning3 clause) actual plan
%   selection method is used to chose one. Then corresponding intention
%   (Parent_Intention, or a a new intention when Parent_Intentin is
%   null) and event are adjusted.
%  @arg Event_ID: event identifier
%  @arg Event_Type: event type
%  @arg Event_Atom: event atom
%  @arg Parent_Intention: Inetntion that raised the event, or null when
%   event is external
%  @arg Context: event context
%  @arg History: used plans for the event so far
%  @arg Means: possible means for the event

% means found, intention is extended (or created)

deliberation4(Event_ID, Event_Type, Event_Atom, Parent_Intention,
                       Context, History, Means):-
    get_intended_means(Means, event(Event_ID, Event_Type, Event_Atom,
                                    Parent_Intention, Context, active,
                                    History),
                       Intended_Means),
% format("INTENDED MEANS: ~w~n",[Intended_Means]),
    extend_intention(Parent_Intention, Intended_Means, Intention_ID),
    update_event(Intention_ID,
	         event(Event_ID, Event_Type, Event_Atom, Parent_Intention,
                       Context, active, History),
                 true, Intended_Means).

% no means found, update event (put it back)

deliberation4(Event_ID, Event_Type, Event_Atom, Parent_Intention,
                       Context, History, _):-
    format(atom(String), 'No means for the event', []),
    println_debug(String, interdbg),
    update_event(-1, event(Event_ID, Event_Type, Event_Atom, Parent_Intention,
                           Context, active, History),
                 false, _).





%===============================================================================
%                                                                              |
%    AGENT CONTROL LOOP                                                        |
%                                                                              |
%===============================================================================

%!  loop(+Steps, -Steps_Left).
%   Agent's control loop. Agent senses, update models for reasoning,
%   makes practical reasoning, executes one act of an intention. The
%   last literal (goal) is next_loop, which checks if the loop
%   continues.

%  agent finished

loop(-1, -1).

%

loop(Steps, Steps_Left):-
    loop_number(Loop_Number),
    format(atom(String1),'
=====================================================================
========================== Loop ~w started ==========================
=====================================================================
~n',[Loop_Number]),
    println_debug(String1, reasoningdbg),

 statistics(global_stack, Global_Stack),
 statistics(trail, Trail),


%    findall(_, clause(Predicate, Arity), Clauses),
%    length(Clauses, Count),
%   writeln(Count),

    format(atom(String_Stat), 'Global stack: ~w ~n Trail: ~w~n', 
                              [Global_Stack, Trail]),
    println_debug(String_Stat, interdbg),

    format(atom(String2), 'STATE IN LOOP ~w~n', [Loop_Number]),
    print_state(String2),

    late_bindings(Bindings),    
    format(atom(String3), 'Bindings ~w~n', [Bindings]),
    println_debug(String3, interdbg),

    format(atom(String4), '~n===~n<< LOOP ~w: SENSING >>~n===', [Loop_Number]),
    println_debug(String4, interdbg),
    sensing,
    !,

    format(atom(String5), '~n===~n<< LOOP ~w: MODEL UPDATE>>~n===', [Loop_Number]),
    println_debug(String5, interdbg),
    update_models,
    !,

    format(atom(String6), '~n===~n<< LOOP ~w: DELIBERATION >>~n===', [Loop_Number]),
    println_debug(String6, interdbg),
    deliberation,
    !,

    format(atom(String7), '~n===~n<< LOOP ~w: ACTING >>~n===', [Loop_Number]),
    println_debug(String7, interdbg),
          
    acting,
    !,

    format(atom(String8), '~n===~n<< LOOP ~w: FINISHED >>~n===', [Loop_Number]),
    println_debug(String8, interdbg),
    increment_loop,
    Steps2 is Steps-1,
    !,
    garbage_all,
    

    next_loop(Steps2, Steps_Left).



%!  next_loop(+Steps, -Steps_Left) is det
%   are we continuing to execute the loop? No, if 1, the counter is at
%   zero, or if there is no active intensity and no declared goal, then
%   we put 'finished', where we goof off and say goodbye
%  @arg Steps: Input number of steps that remains. If it is -1, then
%   step limit is not considered
%  @arg Steps_Left: number of steps that left unused if the agent
%   stopped before the steps limit


% next_loop(-1,-1):-
%    loop(-1,-1).

% steps are over, if 'timeout' terminating is set, then terminate

next_loop(0,0):-
    terminate(timeout),
    print_state('Finished - timeout'),
    finished.

% an intention exists, should go on

next_loop(Steps, Steps_Left):-
    intention(_, _, active),
    !,
    go_sync_agent,
    loop(Steps, Steps_Left).		

% an event exists, should go on

next_loop(Steps, Steps_Left):-
    event( _, _, _, _, _, active, _),
    !,
    go_sync_agent,
    loop(Steps, Steps_Left).         

% if no_job terminating is set, then terminate

next_loop(Steps, Steps):-
    terminate(no_job),
    print_state('Finished - no job'),
    finished.

% no job but does not care, set steps to 1 (or something greater
% than 0)

next_loop( _, Steps_Left):-
    loop( 1, Steps_Left).

garbage_all:-
    garbage_collect,
    garbage_collect_atoms,
    garbage_collect_clauses,
    trim_stacks.



% dont sync in virtual mode

go_sync_agent:-
    virtual_mode(true).


% sync when in synchronous mode

go_sync_agent:-
    thread_self(Agent),
    loop_number(Loop_Number),
    fa_sync:agent_salutes(Agent),
    thread_wait(fa_sync:b_step(Loop_Number), [alias(Agent)]).



%!  increment_loop is multi
%   increases loop_number in Prolog database 

increment_loop:-
    retract(loop_number(Loop_Number)),
    New_Loop_Number is Loop_Number + 1,
    assert(loop_number(New_Loop_Number)).



%!  finished is det
%   Agent finalizer. In this version it just says goodbye.

finished:-
    thread_self(Agent),
    loop_number(Steps),
    format(atom(String), 'Agent ~w finished in ~w steps. ~n',
           [Agent, Steps]),
    println_debug(String, systemdbg).



%===============================================================================
%                                                                              |
%    FORCE REASONING / EXECUTION ( For modelling agent's behaviour)            |
%                                                                              |
%===============================================================================


%!  force_percieving 

force_perceiving:-
    sensing.

%!  force_reasoning(+Model_Reasoning_Node) is det
%   Forces adaption of plan with Plan_ID in Context for specified WEI
%  @arg Model_Node: reasoning node of look-ahead model, defined in
%   FRAgMCTSModel.pl as model_reasoning_node(WEI, Plan_ID,
%   Context)

force_reasoning(Node_ID, 
		model_reasoning_node(
                    event(Event_Index, Event_Type, Event_Atom, Parent_Intention,
                          Event_Context, active, History),
                    Plan,
                    Plan_Context)
               )
:-
    retract(event(Event_Index, Event_Type, Event_Atom, Parent_Intention,
                  Event_Context, active, History)),
    extend_intention(Parent_Intention, [Plan, Plan_Context], Intention_ID2),
    update_event(Intention_ID2,
                 event(Event_Index, Event_Type, Event_Atom, Parent_Intention,
                       Event_Context, active, History),
                 true,
                 [Plan, Plan_Context]).


%!  force_execution(+Model_Act_Node, -Reward) is multi
%   Performs Act. This Act's instance given by Decision.
%  @arg Model_Act_Node: act node of look-ahead model, defined in
%   FRAgMCTSModel.pl as model_act_node(Intention_ID, Act, Decision)
%  @arg Reward: Action reward, if any

% sub-plan finished, just update intention

force_execution(Node_ID, model_act_node(Intention_ID, true, _), 0):-
    intention(Intention_ID, Plan_Stack, Status),
    update_intention(intention(Intention_ID, Plan_Stack, Status), _).


% perform the act

force_execution(Node_ID, model_act_node(Intention_ID, Act, Decision), Reward):-
    retract(intention(Intention_ID,
                      [plan(Plan_ID, Event_Type, Goal_Atom, Conditions, Context,
                            [Plan_Act| Plan_Acts])| Plans],
                      Status)),
% action in node and in the plan could have renamed vars, unify them
    unifiable(Plan_Act, Act, Unifier),
% make act instance
    apply_substitutions(Unifier),
% restrict the original context to get a new context
    restriction(Context, Decision, Context2),
% update intention, the plan now has the new context
    assert(intention(Intention_ID,
                     [plan(Plan_ID, Event_Type, Goal_Atom, Conditions, Context2,
                           [Plan_Act| Plan_Acts])| Plans],
                     Status)),
% execute act, update intention(s)
    execute_plan(Intention_ID,
                 [plan(Plan_ID, Event_Type, Goal_Atom, Conditions, Context2,
                       [Plan_Act| Plan_Acts])| Plans], P2,
                 Result),
    
    check_reward(Result, Reward),
    update_intention(intention(Intention_ID, P2, Status), Result),
    update_intentions(Result).


% in the case acing failed

force_execution(_, model_act_node( _, _, _)).


check_reward(reward(Reward), Reward).

check_reward( _, 0).


%===============================================================================
%                                                                              |
%    AGENT INITIALIZATION, SETTTINGS and LAUNCHING                             |
%                                                                              |
%===============================================================================

%!  clear_agent is det
%   retract all agent parts of its configuration 
%   (used in mcts reasoning)		

clear_agent:-
    retractall(fact(_)),
    retractall(event( _, _, _, _, _, _, _)),
    retractall(plan( _, _, _, _)),
    retractall(plan( _, _, _, _, _)),
    retractall(intention( _, _, _)).



%!  set_clauses(+Clauses, +Plan_ID)
%   Asserts Clauses that are agent's program. When asserting plans,
%   index them with Plan_ID, which is original fresh plan's identifier.
%  @arg Clauses: agent's program
%  @arg Plan_Index: actual index for a plan, if it is added to agent's
%   plan base

set_clauses([],_).

% inserting plan, use the Plan_ID and increment it for possibly next
% declared plan

set_clauses([plan(Event_Type, Goal_Atom, Conditions, Body)| Clauses], Plan_ID)
    :-
    assert(plan(Plan_ID ,Event_Type, Goal_Atom, Conditions, Body)),
    Plan_ID2 is Plan_ID+1,
    set_clauses(Clauses, Plan_ID2).

% inserting top-level goal, it generates corresponding event for it

set_clauses([goal(Event_Type, Goal_Atom, Context)| Clauses], Plan_Index):-
    get_fresh_event_number(Event_Index),
    assert(event(Event_Index, Event_Type, Goal_Atom, null, Context, active,
                 [])),
    set_clauses(Clauses, Plan_Index),
    !.

% the rest (beliefs) are simply asserted

set_clauses([Clause| Clauses], Plan_Index):-
    assert(Clause),
    set_clauses(Clauses, Plan_Index).


%!  delete_clauses is det

delete_clauses:-
    retractall(plan(_, _, _, _, _)),
    retractall(event(_, _, _)),
    retractall(intention(_, _, _)),
    retractall(event(_, _, _, _, _, _, _)).

%!  load_program(+Filename, -Clauses) is multi
%   Loads agent program from specified file
%  @arg Filename: Name of agent's program file
%  @arg Causes: List of loaded clauses

load_program(Filename, Clauses):-
    access_file(Filename, read),! ,
    open(Filename, read, Stream, [close_on_abort(true)]),
% reads the first Clause from the Stream
    read_clause(Stream, Clause, []),
    !,
    read_clauses(Clause, Clauses, Stream),
    close(Stream, [force(true)]).

load_program(Filename, []):-
    format(atom(String),'Agent file ~w cannot be opened~n',
           [Filename]),
    println_debug(String, error),
    !,
    fail.


%!  read_clause(+Clause, -Clauses, +Stream) is multi
%  @arg Clause: The first clause as input (to check end_of_file)
%  @arg Clauses: List of Clauses in Stream
%  @arg Stream: File Stream

read_clauses(end_of_file, [], _):- !.

read_clauses(Clause, [Clause|Clauses], Stream):-
    read_clause(Stream, Clause2, []),
    read_clauses(Clause2, Clauses, Stream).


%!  take_snapshot(+Snapshot)
%   Wraps agent's state to one list
%  @arg Snapshot: snapshot of agent state

take_snapshot(Snapshot):-
    take_snapshot_beliefs(Beliefs_Snapshot),
    take_snapshot_goals(Events_Snapshot),
    take_snapshot_plans(Plans_Snapshot),
    take_snapshot_intentions(Intentions_Snapshot),
    append([Beliefs_Snapshot, Events_Snapshot, Plans_Snapshot,
            Intentions_Snapshot], Snapshot).

%!  take_snapshot_intention(+Intentions_Snapshot) is det

take_snapshot_intentions(Intentions_Snapshot):-
    findall(intention(Number, PlanStack, Status),
          intention(Number, PlanStack, Status), Intentions_Snapshot).

%!  take_snapshot_intention(+Plans_Snapshot) is det

take_snapshot_plans(Plans_Snapshot):-
    findall(plan(Number, Type, Predicate, Context, Body),
          plan(Number, Type, Predicate, Context, Body), Plans_Snapshot).

%!  take_snapshot_beliefs(+Beliefs_Snapshot) is det

take_snapshot_beliefs(Beliefs_Snapshot):-
    findall(fact(X), fact(X), Beliefs_Snapshot).

%!  take_snapshot_events(+Events_Snapshot) is det

take_snapshot_goals(Events_Snapshot):-
    findall(event(Event_Index, Type, Predicate, Intention, Context, Status,
                History),
	  event(Event_Index, Type, Predicate, Intention, Context, Status,
                History),
	  Events_Snapshot).


/*

%!  wait_go(Trigger) is multi
%   Synchronizes agent's execution. At this distribution it is used only to
%   make active waiting barrier at the start of the multiagent system 
%  @arg Trigger: 0 ... cancel execution, 1 ... go on with agent execution

wait_go( _ ):-
    fa_sync:go(0),
    thread_exit(1).

wait_go(Trigger):-
    fa_sync:go(Trigger),
    !.

wait_go(Trigger):-
    writeln(wait_go),
    wait_go(Trigger).

*/


%!  synchronous start of all the agents in system 
%   Synchronized by shared atom on FRAg's blackboard

go_sync(Steps, I):-
    thread_self(Agent),
    fa_sync:agent_salutes(Agent),
    thread_wait(fa_sync:b_step(I), [alias(Agent)]),
    call_time(loop(Steps, Steps_Left),Time),
    get_dict(cpu, Time, Cpu_Time),
    thread_self(Agent),
    timeout(Max_Iterations),
    Steps_Total is Max_Iterations - Steps_Left,
    write_stats(stats(Agent, Cpu_Time, Steps_Total)),
    assert(fa_sync:ready(Agent)).


fa_init_com(Filename):-
    thread_self(Agent),
    format(atom(Filename2), '~w_~w.out', [Filename, Agent]),
    tell(Filename2),
    assert(agent_debug(1)),
    !.

fa_finalize_com:-
    told.



%!  set_control(+Terminating) is det
%   Sets Terminanting mode. Agents can finish their doing when there is no more
%   Intention or Event to process, or after a certain number of execution 
%   cycles. or runs continuously until externally terminated.
%  @Terminating: timeout, Steps / no_job / never ... others = never

set_control(terminate(timeout, Steps)):-
    retract(terminate( _ )),
    assert(terminate(timeout)),
    retract(timeout( _ )),
    assert(timeout(Steps)).

set_control(terminate(Terminating)):-
    retract(terminate( _ )),
    assert(terminate(Terminating)).


% get_default_environments(Environments):-
%    bagof(Environment, default_environment(Environment), Environments).
%
% get_default_environments([]).



%!  set_default_environment(+Environment) is det
%   Declares the environment to be used by all agents by default. The 
%   environment must already be loaded on the system.
%  @arg Environment: environment to be assignet defaultly to the agents

set_default_environment(Environment):-
    % such environment is loaded
    environment_loaded(Environment),
    assert(default_environment(Environment)).

set_default_environment(Environment):-
    format(atom(String),"Environment '~w' does not exists~n",
           [Environment]),
    println_debug(String, error).



%!  set_late_bindings(+Bindings) is det
%   Sets active bindings strategy to Bindigs (true -> late, false -> early)
%  @arg Bindings: Bindings strategy, (true -> late, false -> early)

set_late_bindings(Bindings):-
    retractall(late_bindings( _ )),
    assert(late_bindings(Bindings)).



%!  set_late_bindings is det
%   Sets active bindings strategy to 'late'

set_late_bindings:-
    retractall(late_bindings( _ )),
    assert(late_bindings(true)).



%!  set_early_bindings is det
%   Sets active bindings strategy to 'early'

set_early_bindings:-
    retractall(late_bindings( _ )),
    assert(late_bindings(false)).



%!  set_default late_bindings(+Bindings) is det
%   Sets default bindings strategy to Bindigs 
%   When agent starts, this bindings strategy will be set to active
%  @arg Bindings: Bindings strategy, (true -> late, false -> early)

set_default_late_bindings(Bindings):-
    retractall(default_late_bindings( _ )),
    assert(default_late_bindings(Bindings)).



%!  is_late_bindings
%   Succeed, if active bindings strategy is 'late'

is_late_bindings:-
    late_bindings(true).



%!  is_default%late_bindings
%   Succeed, if default bindings strategy is 'late'

is_default_late_bindings:-
    default_late_bindings(true).



%!  set_environment(+Environment) is det
%   Situates the agent in the main instance of the Environment
%  @Environment: Environment name

set_environment(Environment):-
    thread_self(Agent),
    fRAgAgentInterface:situate_agent(Agent, Environment),
    format(atom(String), 'Agent ~w is situated to environment ~w ~n',
           [Agent, Environment]),
    println_debug(String, systemdbg).



init_intention_selection( _ ):-
    active_intention_selection( _ ).

init_intention_selection(Default_Intention_Selection):-
    set_intention_selection(Default_Intention_Selection).


init_plan_selection( _ ):-
    active_plan_selection( _ ).

init_plan_selection(Default_Plan_Selection):-
    set_plan_selection(Default_Plan_Selection).


init_substitution_selection( _ ):-
    active_substitution_selection( _ ).

init_substitution_selection(Default_Substitution_Selection):-
    set_substitution_selection(Default_Substitution_Selection).



fa_set_reasoning:-
    default_intention_selection(Intention_Selection),
    default_plan_selection(Plan_Selection),
    default_substitution_selection(Substitution_Selection),
    init_intention_selection(Intention_Selection),
    init_plan_selection(Plan_Selection),
    init_substitution_selection(Substitution_Selection),
    !.

fa_set_reasoning:-
    format(atom(STRING),"Unspecified default reasoning mehods~n", []),
    println_debug(STRING, error),
    !,
    fail.


fa_init_reasoning:-
    active_intention_selection(Intention_Selection),!,
    active_plan_selection(Plan_Selection),!,
    active_substitution_selection(Substitution_Selection),
    init_reasoning(Intention_Selection),
    init_reasoning(Plan_Selection),
    init_reasoning(Substitution_Selection).

fa_init_reasoning:-
    format(atom(STRING),"Reasoning methods initialization failed~n",
           []),
    println_debug(STRING, error),
    !,
    fail.




fa_init_environments:-
    bagof(Environment, default_environment(Environment), Environments),
    !,
    fa_init_environments2(Environments).

fa_init_environments:-
    thread_self(Agent),
    format(atom(STRINGS), "No environment for agent ~w~n", [Agent]),
    println_debug(STRINGS, systemdbg).


fa_init_environments2([]).

fa_init_environments2([Environment| Environments]):-
    set_environment(Environment),
    !,
    fa_init_environments2(Environments).

fa_init_environments2([Environment| Environments]):-
    format(atom(STRING),"Environment '~w' initialization failed~n",
           [Environment]),
    println_debug(STRING, error),
    fa_init_environments2(Environments).



%!  fa_init_run is det
%   Set bindings strategy and resets 'fresh' identifiers

fa_init_run:-
    retractall(late_bindings( _ )),
    default_late_bindings(Bindings),
    assert(late_bindings(Bindings)),
    retractall(loop_number( _ )),
    retractall(intention_fresh( _ )),
    retractall(event_fresh( _ )),
    assert(loop_number(1)),
    assert(intention_fresh(1)),
    assert(event_fresh(1)).

fa_init_run:-
    format(atom(String),"Bindings method missing~n", []),
    println_debug(String, error),
    !,
    fail.                              



%!  fa_init_process_attrs(+Attributes) is det
%   Process list of attributes, attribute is specified in tuple (key, value)
%   Possible attributes are specified in @todo documentation
%  @arg Attributes: List of attributes

fa_init_process_attrs([]).

fa_init_process_attrs([(Key, Value)| Attributes]):-
    fa_init_set_attrs(Key, Value),
    !,
    fa_init_process_attrs(Attributes).


fa_init_set_attrs(environment, Environment):-
    thread_self(Agent),
    situate_agent(Agent, Environment).

fa_init_set_attrs(environment, Environment):-
    thread_self(Agent),
    format(atom(String),
           "Failed assignment of envrironment ~w to agent ~w",
           [Environment, Agent]),
    println_debug(String, error).

fa_init_set_attrs(reasoning, Reasoning):-
    set_reasoning(Reasoning).

fa_init_set_attrs(debug, Debug):-
    assert(agent_debug(Debug)).

fa_init_set_attrs(bindings, late):-
    set_late_bindings(true).

fa_init_set_attrs(bindings, early):-
    set_late_bindings(false).

fa_init_set_attrs(Key, Value):-
    format(atom(String), "Wrong attributes (~w:~w)~n", [Key, Value]),
    println_debug(String, error).



%!  fa_init_agent(+Filename, +Attributes) is det
%   Loads agent from specified file, set default properties (attributes, 
%   reasonings etc.)
%  @arg Filename: 'fap' file with agent program
%  @arg Attributes: agent's attributes specified in mas2fp metafile 

fa_init_agent(Filename, Attributes):-
    assert(virtual_mode(false)),
    timeout(Iterations),
    string(Filename),
    format(atom(Filename2), "~w.fap", [Filename]),
    load_program(Filename2, Clauses),
    assert(agent_debug(error)),
    fa_init_com(Filename),
    fa_init_run,
    fa_set_reasoning,
    fa_init_process_attrs(Attributes),
    fa_init_environments,
    fa_init_reasoning,
    set_clauses(Clauses, 1),
    go_sync(Iterations, 1),
    fa_finalize_com,
    thread_exit(1).

fa_init_agent( _, _):-
    go_sync(-1, _),		% born dead
    thread_self(Agent),
    format(atom(String), "Agent ~w initialization failed~n",
           [Agent]),
    println_debug(String, error),
    fa_finalize_com,
    thread_exit(1).
