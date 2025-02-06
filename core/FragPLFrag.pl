
/**

This file is part of the FRAg program. It is insluded into agent's file 
FRAgAgent.pl. Contains clauses that are specific to the late variable binding 
system when interpreting agent code.

@author Frantisek Zboril
@version 2021 - 2024
@license GPL

*/



% reasoning - plan selection + substitution selection (decide op.) + intention
% selection

:-thread_local active_intention_selection /1.
:-thread_local active_plan_selection /1.
:-thread_local active_substitution_selection /1.

:-dynamic default_intention_selection /1.
:-dynamic default_plan_selection /1.
:-dynamic default_substitution_selection /1.


%===============================================================================
%                                                                              |
%	Reasoning methods - intention, plan and substitution selections        |
%	Setting default and actual strategies (methods)                        |
%       Getting actuaul strategies                                             |
%                                                                              |
%===============================================================================

% In this version of FRAg, the actual strategy setting is only used for MCTS 

set_intention_selection(Intention_Selection):-
% checks if such reasoning exists
    reasoning_method(Intention_Selection),
    retractall(active_intention_selection( _ )),
    assert(active_intention_selection(Intention_Selection)),
    format(atom(String),
           "[SYSDBG] Setting intention selection method to ~w~n",
           [Intention_Selection]),
    println_debug(String, systemdbg).


set_intention_selection(Intention_Selection):-
    format(atom(String), "Intention selection cannot be switched to ~w~n",
	   [Intention_Selection]),
    print_debug(String, error).


set_default_intention_selection(Intention_Selection):-
    reasoning_method(Intention_Selection),
    retractall(default_intention_selection( _ )),
    assert(default_intention_selection(Intention_Selection)),
    format(atom(String),
           "[SYSDBG] Setting default intention selection method to ~w~n",
           [Intention_Selection]),
    println_debug(String, systemdbg).

set_default_intention_selection(Intention_Selection):-
    format(atom(STRING),
           "Default intention selection cannot be switched to ~w~n",
           [Intention_Selection]),
    print_debug(STRING, error).


set_plan_selection(Plan_Selection):-
    reasoning_method(Plan_Selection),
    retractall(active_plan_selection( _ )),
    assert(active_plan_selection(Plan_Selection)),
    format(atom(String), "[SYSDBG] Setting plan selection method to ~w~n",
           [Plan_Selection]),
    println_debug(String, systemdbg).


set_plan_selection(Plan_Selection):-
    format(atom(String), "[ERROR] Plan selection cannot be switched to ~w~n",
	   [Plan_Selection]),
    println_debug(String, error).

set_default_plan_selection(Plan_Selection):-
    reasoning_method(Plan_Selection),
    retractall(default_plan_selection( _ )),
    assert(default_plan_selection(Plan_Selection)),
    format(atom(String),
           "[SYSDBG] Setting default plan selection method to ~w~n",
           [Plan_Selection]),
    println_debug(String, systemdbg).

set_default_plan_selection(Plan_Selection):-
    format(atom(String),
           "[ERROR] Default plan selection cannot be switched to ~w~n",
           [Plan_Selection]),
    println_debug(String, error).


set_substitution_selection(Substitution_Selection):-
    reasoning_method(Substitution_Selection),
    retractall(active_substitution_selection( _ )),
    assert(active_substitution_selection(Substitution_Selection)),
    format(atom(String),
           "[SYSDBG] Setting substitution selection method to ~w~n",
           [Substitution_Selection]),
    println_debug(String, systemdbg).


set_substitution_selection(Substitution_Selection):-
    format(atom(String),
	   "[ERROR] Substitution selection cannot be switched to ~w~n",
	   [Substitution_Selection]),
    print_debug(String, error).


set_default_substitution_selection(Substitution_Selection):-
    reasoning_method(Substitution_Selection),
    retractall(default_substitution_selection( _ )),
    assert(default_substitution_selection(Substitution_Selection)),
    format(atom(String), "[SYSDBG] Setting default decision method to ~w~n",
           [Substitution_Selection]),
    println_debug(String, systemdbg).

set_default_substitution_selection(Substitution_Selection):-
    format(atom(String),
           "[ERROR] Default substitution selection cannot be switched to ~w~n",
           [Substitution_Selection]),
    print_debug(String, error).



set_reasoning(Reasoning):-
    reasoning_method(Reasoning),
    set_intention_selection(Reasoning),
    set_plan_selection(Reasoning),
    set_substitution_selection(Reasoning).

set_reasoning(Reasoning):-
    format(atom(String), "[ERROR] Reasoning cannot be switched to ~w~n",
           [Reasoning]),
    print_debug(String, error).



set_default_reasoning(Reasoning):-
    reasoning_method(Reasoning),
    set_default_intention_selection(Reasoning),
    set_default_plan_selection(Reasoning),
    set_default_substitution_selection(Reasoning).

set_default_reasoning(Reasoning):-
    format(atom(String),
	   "[ERROR] Default reasoning cannot be switched to ~w~n", [Reasoning]),
    print_debug(String, error).


%

get_default_reasoning(Intention_Selection, Plan_Selection,
                      Substitution_Selection):-
    default_intention_selection(Intention_Selection),
    default_plan_selection(Plan_Selection),
    default_substitution_selection(Substitution_Selection).



%===============================================================================
%                                                                              |
%	Model ...        |
%                                                                              |
%                                                                              |
%===============================================================================



%!  update_models is det
%   
%   

update_models:-
    active_intention_selection(Intention_Selection),
    active_plan_selection(Plan_Selection),
    active_substitution_selection(Substitution_Selection),
    list_to_set([Intention_Selection, Plan_Selection, Substitution_Selection],
                Models),
    !,
    update_models2(Models).

update_models.


update_models2([]).
        
update_models2([Model| Models]):-
    update_model(Model),			% v jednotlivych reasoninzich
    update_models2(Models).



%!  get_intention(+Intentions, -Intention) is nondet // depends on strategy
%  @Intentions: an input set of intentions
%  @Intention: selected intention
%   Selects one of the intention according to how the intention selection 
%   strategy is set up

get_intention(Intentions, intention(Intention_ID, Context, Plan_Stack)):-
    active_intention_selection(Intention_Selection),
    get_intention(Intention_Selection, Intentions,
                  intention(Intention_ID, Context, Plan_Stack)),
% intention(INTENTIONINDEX, CONTEXT, PLANSTACK2),
    loop_number(Loop),
    format(atom(String),
           "~n[RSNDBG] GET INTENTION [~w / ~w] -> ~w~n",
           [Loop, Intention_Selection,
               intention(Intention_ID, Context, Plan_Stack)]),
    print_debug(String, reasoningdbg).



get_intended_means(Means, Event, Intended_Means):-
    active_plan_selection(Plan_Selection),
    get_plan(Plan_Selection, Event, Means, Intended_Means),
    loop_number(Loop),
    format(atom(String),
           "~n[RSNDBG] GET PLAN [~w / ~w] -> FOR ~w ~n[......] -> PLAN ~w~n",
		   [Loop, Plan_Selection, Event, Intended_Means]),
    print_debug(String, reasoningdbg).


% TODO get_substitution is missing

%===============================================================================
%                                                                              |
%    FRAg Methods ... for work with substitutions                              |
%    broad_unification							       |
%    instance_set                                                              |
%    restriction                                                               |
%    shorting                                                                  |
%    inersection                                                               |
%    decisioning						               |
%                                                                              |
%===============================================================================



%!  broad_unification(+Atom, +Atoms, -PUS_Out) is det
%  @arg Atom: query to a base
%  @arg Atoms: the base
%  @arg PUS_Out: Possible unifier set for Atom and Atoms
%   Performs broad unification of Atom in Atoms (Belief base). Resulti is
%   a set of substitutions in PUS_Out

broad_unification(Atom, Atoms, PUS_Out):-
    broad_unification2(Atom, Atoms, PUS),
    sort(PUS, PUS_Out).


broad_unification2(_ ,[], []).

broad_unification2(Atom1, [Atom2| Atoms], [Substitution_Out| Substitutions])
    :-
% Substitution1 = mgu(G,BELIEF)
    unifiable(Atom1, Atom2, Substitution1),
% Substitution2 is SUBSTITUTION without renamings
    remove_renamings(Substitution1, Substitution_Out),
% next mgu for the next belief
    broad_unification2(Atom1, Atoms, Substitutions).

broad_unification2(Atom, [_ | Atoms], Substitutions):-
    broad_unification2(Atom, Atoms, Substitutions).



%!  remove_renamings(+Bindings_In, -Bindings_Out) is det
%  @arg Bindings_In: input bindings
%  @arg Bindings_Out: output bindings
%   Removes renamings (bindings with two variables) from Bindings_In and the
%   result is in Bindings_Out

remove_renamings([],[]).

remove_renamings([A=B| T1], T2):-
    var(A),var(B),
    remove_renamings(T1, T2).

remove_renamings([H| T1],[H| T2]):-
    remove_renamings(T1, T2).



%!  instance_set(+Atom, +Substitutions, -Atoms) is det
%   Creates a set of instances of Atom by applying substitutions from PUS
%  @arg Atom:
%  @arg Substitutions: set of substitutions (PUS) 
%  @arg Atoms: Atoms that are created by applying PUS substitutions to the Atom

instance_set(_ ,[],[]).

instance_set(Atom, [Substitution| Substitutions], Instance_Set):-
    copy_term([Atom| Substitution], [New_Atom| New_Substitution]),
    apply_substitutions(New_Substitution),
    instance_set(Atom, Substitutions, Substitutions2),
    sort([New_Atom| Substitutions2], Instance_Set).



apply_substitutions([]).					%

apply_substitutions([Binding| Bindings]):-
    Binding,
    apply_substitutions(Bindings).



%!  restriction(+Substitutions1, +Substitutions2, -Substitutions_Out) is det
%
%  @arg Substitutions1
%  @arg Substitutions2
%  @arg Substitutions_Out

restriction([[]],[[]],[[]]).    % empty PUS's restriction

restriction([Substitution1| Substitutions1], PUS2, PUS_Out):-
    restriction2(Substitution1, PUS2, PUS3),
    restriction(Substitutions1, PUS2, PUS4),
    append_not_empty(PUS3, PUS4, PUS_Out).

restriction( _, _, []).



restriction2(_,[],[[]]).

restriction2(Substitution1, [Substitution2| Substitutions], PUS_Out):-
    merging(Substitution1, Substitution2, Substitution3),
    restriction2(Substitution1, Substitutions, PUS1),
    append_not_empty([Substitution3], PUS1, PUS_Out).



%!  merging()
%	MERGING
%       PUMerged = PU1 m PU2 =def ... see [1]
%	merging(PU1,PU2,PUMerged).
%

merging(Substitution1, Substitution2, Substitution_Out):-
    merging2(Substitution1, Substitution2),
    append(Substitution1, Substitution2, Substitution3),
    sort(Substitution3, Substitution_Out).

merging(_,_,[]).



merging2(_, []).

merging2(Substitution, [Bind| Bindings]):-
    merging3(Substitution, Bind),
    merging2(Substitution, Bindings).



% merging3(substitutionsList, substitions) is true, when the same variables in
% both substitutions are mapped to the same terms/atoms

merging3([], _).

merging3([A=B| Bindings], C=D):-
    A==C, !,
    B=D,
    merging3(Bindings, C=D).

merging3([_| Bindings], Bind):-
    merging3(Bindings, Bind).



%!
%   appends two PUSs, but when one contains just one empty substitution,
%   this empty is not appended to another (is ignored)

append_not_empty([[]], List, List).

append_not_empty(List, [[]], List).

append_not_empty(List1, List2, List3) :- append(List1, List2, List3).


%
% SHORT  // neodpovida clanku, tady se z PUS jen vytahnou promenne
% musi se , vzit promenne, vzit PUS a vytvorit novy PUS a s novymi promennymi
% pokud mame PUS s prazdnymi unifikatory, udelame z toho jen jedno PUS
% [[],[],...] -> [[]]
%

set_empty_PUS([[]|_], [[]]).

set_empty_PUS(PUS, PUS).


% memberVar(A=B,VARS)
% is A from the variable list VARS?

memberVar(A=_, [C|_]):- A==C.

memberVar(Bind, [_| Bindings]):- memberVar(Bind, Bindings).



% ! shorting(+Goal1, +Goal2, +PUS1, +Vars1, -PUS2, -Vars2) is det
%  @arg Goal1 vstupni cil
%  @arg Goal2 vystupni cil
%  @arg PUS1
%  @arg Vars1
%  @arg PUS2
%  @arg Vars2
%
% vstupnicl -> vystupnicil
%, ten samy cil ale s radne prejmenovanymi promennymi
%
%   no variables -> empty PUS (bez toho to tuhne, coz by nemelo)
%   shorting(_,_,IPUS,[],[[]],[]).
%

shorting(Goal1, Goal2, PUS_In, Vars_In, PUS_Out, Vars_Out):-
% nashortujeme IPUS podle promennych v IV, vysledek v SPUS
    shorting_pus(PUS_In, Vars_In, PUS1),
% pokud je vystup [[],[],....] udelame z nej pouze [[]]
    set_empty_PUS(PUS1, PUS2),
% must rename to get fresh variable names
    copy_term([Goal1, PUS2],[Goal2, PUS_Out]),
% a vezmem jen promenne, ktere jsou v novem PUS  (promennych bylo vic,
% nez bylo ve vstupnim PUS) shorting [[A=a],[B=b]],[A,C] ->  [[A=a]],[A]
    term_variables(PUS_Out, Vars_Out).



%!  shorting(Substitutions_In, Var_List, Substitutions_Out) is det
%   basic shorting: substitution x list of variables -> substitutions just
%   for these variables

shorting_pus([],_,[]).

%   shorts every substitution from the first list with respect with the list of
%   variables V (only V-pairs remains)

shorting_pus([H1|T1],V,[H2|T2]):-
    shorting(H1,V,H2),
    shorting_pus(T1,V,T2).



%!  shorting(+Substitution, +Variables, -Substitution) is ...?
% TODO ASAP

%   empty context remains empty context
shorting([], _, []).
%   no variable -> empty context
shorting(_, [], []).

%   shorts one substitution due to the variables in VARS
%   [A->a,B->,D->d,F->d],[A,D] -> [A->a,D->d]

shorting([Bind| Bindings], Variables, [Bind| Bindings2]):-
    memberVar(Bind, Variables),
    shorting(Bindings, Variables, Bindings2).

shorting([_| Bindings], Variables, Bindings2):-
    shorting(Bindings, Variables, Bindings2).


%
%   After execution PUS may include weird tuples constant=constant even for a
%   pair with distinc constants these should be reducet, or better PUS should 
%   be reduced only to those mapping variables to a term
%	shortNoVars(PUS, PUS).
%


short_variables_binds2([], _, []).

short_variables_binds2([Substitution| Substitutions], Variables, [PUS| PUSs]):-
    shorting(Substitution, Variables, PUS),
    short_variables_binds2(Substitutions, Variables, PUSs).

short_variables_binds([Substitution| Substitutions], PUSs):-
    term_variables(Substitution, Variables),
    short_variables_binds2([Substitution| Substitutions], Variables, PUS),
    sort(PUS, PUSs).



%!  intersection(+Atom1, +Context_In, +Atom2, -Context_Out) is det
%   Atom1, Context_In ~ Atom2 =def BU(IS(Atom1, Context), Atom2)
%   @see [1] (Late Bindings) ...
%  @arg Atom1: original plan goal atom
%  @arg Context_In: original plan context
%  @arg Atom2: new plan triggering event atom

intersection(Goal_Atom1, PUS1, Goal_Atom2, PUS2):-
    instance_set(Goal_Atom1, PUS1, Instance_Set1),
    broad_unification(Goal_Atom2, Instance_Set1, Answers),
    term_variables(Goal_Atom2, Goal_Atom2_Variables),
    shorting_pus(Answers, Goal_Atom2_Variables, PUS2).



%!  intersection(+Atom1, +Context1, +Atom2, +Context2, -Context_Out) is det
%   Atom1, Context_In ~ Atom2, Context2 =def Restriction(
%                                                BU(IS(Atom1, Context1), Atom2), 
%                                                Context2
%						)
%   @see [1] (Late Bindings) ...
%  @arg Atom1: upper plan goal atom
%  @arg Context1: upper plan context
%  @arg Atom2: sub-plan triggering event atom

intersection(Goal_Atom1, Context1, Goal_Atom2, Context2, Context_Out):-
    intersection(Goal_Atom1, Context1, Goal_Atom2, Context3),
    restriction(Context3, Context2, Context_Out).



%!  decide_context(+Atom, +Context_In, +Variables, +Context_Out) is nondet
%  @arg Atom:
%  @arg Context_In:
%  @arg Variables:
%  @arg Context_Out:    

decide_context( _, _, [], []).		% no vars / nothing to decide

decide_context(Atom, Context, Variables, Context2):-
    active_substitution_selection(Substitution_Selection),
% get_substitution is implemented in particular reasoning files (included by
% the system during FRAg initialization.
    get_substitution(Substitution_Selection, Atom, Context, Variables, 
                     Context2),
    loop_number(Loop),
    format(atom(String),
           'GET DECISION [~w / ~w] ->~n[......] FOR ~w ~w
[......] DECISION -> ~w~n',
		   [Loop, Substitution_Selection, Atom, Context, Context2]),
	print_debug(String, reasoningdbg).



%!  decisioning(+Action_Term, +Context, -Context_Out) is nondet
%   Takes the first PUS from the context / simple reasoning
%   selects one substitution from the context due to requestet variebles
%   'to ground' all the substitutions in the context must soud to the variables
%   bindings in the selected context
%  @arg Action_Term:
%  @arg Context:
%  @arg Context_Out:

%   uses reasoning method due to active_reasoning_method(-Method)
%

decisioning(Action_Term, Context, Context_Out, Apply):-
    term_variables(Action_Term, Action_Variables),
    decide_context(Action_Term, Context, Action_Variables, Context2),
    restriction(Context, [Context2], Context_Out),
    do_apply(Apply, Context2).

do_apply(false, _).

do_apply(true, Context):-
    apply_substitutions(Context).



%
%	Queries
%

%!  simulate_early_bindings(+Atom, +Context_In, -Context_Out, 
%			    +Late, +Apply) is nondet
%   If Context_In is non-empty and the early binding strategy is active, it 
%   decides how the free variables in the Atom will be bound. It selects one
%   of the substrings from Context_In and modifies the context on Context_Out
%   to respect the chosen bindings.
%  @arg Atom: input Atom
%  @arg Context_In: input Context
%  @arg Late: true / false -> early / late bindings
%  @arg Context_Out: output Context
%  @arg Apply: apply substituions, instantiate variables

% simulate_early_bindings( _, [], []).

%   late bindings are set, so do not simulate early bindings

simulate_early_bindings( _, Context, Context, true, _).


%   already failed
simulate_early_bindings( _, [], [], _, _).


simulate_early_bindings(Act_Atom, Context_In, Context_Out, false, Apply):-
    decisioning(Act_Atom, Context_In, Context_Out, Apply).



% with no explicit bindings setting 

query(Query, Context, Context_Out):-
   late_bindings(Bindings),
   query(Query, Context, Context_Out, Bindings).


%!  query(+Query, +Context, -Context_Out).
%   Queries agent's base by literal Query
%  @arg Query: query literal (Atom od not(Atom))
%  @arg Context: query input context
%  @arg Context_Out: query output context, empty list if query failed

%  negative literal

query(not(Query), Context, Context_Out, Bindings):-
    query(Query, Context, Context1, Bindings),
%  remove all supersets of some element from Context1 
    substract_subsubstitions(Context, Context1, Context_Out).

query(not( _ ), Context, Context, _).   % query to QUERY failed (no answer)


%  positive literal

query(Query, Context, Context_Out, Bindings):-
    bagof(fact(Query), fact(Query), Answers),
% get list of subsitutions for fact(Query) and Answers
    broad_unification(fact(Query), Answers, Context2),
% in the case of early bindings, chose on subsitution from Context2
    restriction(Context, Context2, Context3),
    simulate_early_bindings(Query, Context3, Context_Out, Bindings, false).

query( _, _, [], _).



%!  substract_subsubstitions(+Substitutions1, +Substitutions2, 
%                     -Substitutions_Out) is det
%   Removes every substitution from Substitutions1 that is a subset of some
%   substitution from Substitutions2
%  @arg Substitutions1: Input substitutions (to be reduced)
%  @arg Substitutions2: Substitutions which supersets should be removed
%  @arg Substitutions_Out: Output substitutions

substract_subsubstitions(Substitutions, [], Substitutions).

substract_subsubstitions(Substitutions1, [Substitution | Substitutions2T],
                         Substitutions_Out)
    :-
    substract_subsubstitions2(Substitutions1, Substitution, Substitutions3),
    substract_subsubstitions(Substitutions3, Substitutions2T,
                             Substitutions_Out).



substract_subsubstitions2([], _, []).

substract_subsubstitions2([Substitution1 | Substitutions1], Substitution2,
                          Substitutions_Out)
    :-
    subsubstitution(Substitution1, Substitution2),
    substract_subsubstitions2(Substitutions1, Substitution2,
                              Substitutions_Out).

substract_subsubstitions2([Substitution1 | Substitutions1], Substitution2,
                          [Substitution1 | Substitutions_Out])
    :-
    substract_subsubstitions2(Substitutions1, Substitution2,
                              Substitutions_Out).


subsubstitution([], _).

subsubstitution(Substitution, [Binding | Bindings]):-
    delete_binding(Substitution, Binding, Substitution2),
    subsubstitution(Substitution2, Bindings).


delete_binding([], _, []).

delete_binding([A=B| Bindings], C=D, Bindings_Out):-
    A==C, B==D,
    delete_binding(Bindings, C=D, Bindings_Out).

delete_binding([Binding | Bindings], Binding2, [Binding | Bindings_Out]):-
    delete_binding(Bindings, Binding2, Bindings_Out).


