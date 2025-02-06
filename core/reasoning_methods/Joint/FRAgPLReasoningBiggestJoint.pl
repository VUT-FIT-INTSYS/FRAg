%
%	Reasoning methods - taking biggest joint action
%	Frantisek Zboril jr. 2022 - 2023
%

%
% Should define
%    get_intention(+Reasoning_type,+Intentions,-Intention).
%
% not defined:
%   get_substitution(+Reasoning_type, +Action_Term, +Substitution_List,
%   +Variable_List,-Substitution_List).
%   get_plan(+Reasoning_Type, +Goal, +RelAppPlans,-Intended_Means).
% if required, 'simple_reasoning' is used.
%

%	This module is loaded / included in the FRAgAgent file


:-thread_local model_action /1.
:-thread_local model_intention /1.

reasoning_method(biggest_joint_reasoning).


%!  most_freq_action(+Action, +Actions, +Number, -Actions_Freq) is det
%  @Action
%  @Actions
%  @Number
%  @Actions_Freq


% there is only one action for processing
most_freq_action(Action, [], _ , [ac(1, Action)]).

% the last action in the list is the same as the actual one
most_freq_action(Action, [Action], Number, [ac(Number2, Action)])
    :-
    Number2 is Number+1.

% the last action in the list is different from the actual one
most_freq_action(Action, [Action2], Number, [ac(Number, Action), ac(1, Action2)]).

most_freq_action(Action, [Action| Actions], Number, AOUT):-
    Number2 is Number+1,
    most_freq_action(Action, Actions, Number2, AOUT).

most_freq_action(Action, [Action2|Actions2], Number, [ac(Number, Action)| Action_Out])
    :-
    most_freq_action(Action2, Actions2, 1, Action_Out).



get_actions([],[]).

get_actions([intention(_ ,[plan(_ ,_ ,_ ,_ , Context,[act(Act)|_])|_ ],active)|
	                   Intentions], Acts_Out):-
    functor(Act, Predicate, Arity),
    is_joint_action( Predicate, Arity),
    instance_set(Act, Context, Acts),
    get_actions(Intentions, Actions),
    append(Acts, Actions, Acts_Out).

get_actions([intention(_ ,[plan(_ ,_ ,_ ,_ ,_ ,[_ |_ ])|_ ],active)| Intentions],
	    Acts)
    :-
    get_actions(Intentions, Acts).

get_actions([intention(_ , Intention, Acts)]):-  %??? tady je neco spatne
    get_actions(Intention, Acts).



get_action([], ac(0,null)).

get_action(Actions, Action):-
    msort(Actions, [Action2| Actions2]),
    most_freq_action(Action2, Actions2, 1, A),
    sort(A, AS),
    reverse(AS,[Action| _]),
    format(atom(String), "[JOINT] ACTIONS GATHERED: ~w~n", [Actions]),
    print_debug(String, jointdbg),
    format(atom(String2), "[JOINT] ACTION CHOSEN: ~w~n", [Action]),
    print_debug(String2, jointdbg).



get_intention2(null, Intentions, Intention):-
    get_intention(simple_reasoning, Intentions, Intention).

get_intention2(Act,_,
	       intention(Intention_ID,
			 [plan(Plan_ID, Event_Type, Event_Term, Conditions,
			       Context,[act(Act)| Acts])| Plans], active))
    :-
    get_intention(simple_reasoning,_ ,_ ),
    intention(Intention_ID,
	      [plan(Plan_ID, Event_Type, Event_Term, Conditions, Context,
		    [act(Act)|Acts])| Plans], active).



get_not_joint_action([intention(_ ,[plan(_ ,_ ,_ ,_ ,_ ,[act(Action)|_ ])|_ ],
				active)|
		      Intentions],
		     Intention)
    :-
    functor(Action, Predicate, Arity),
    is_joint_action(Predicate, Arity),
    !,
    get_not_joint_action(Intentions, Intention).

get_not_joint_action([Intention| _ ], Intention).



get_model_intention(biggest_joint_reasoning, Intentions, Intention):-
    format(atom(String), "[JOINT] INTENTIONS: ~w~n", [Intentions]),
    print_debug(String, jointdbg),
    get_actions(Intentions, Actions),
    get_action(Actions, ac(_, Act)),
    format(atom(String2), "[JOINT] ACTIONS2: ~w~n", [Act]),
    print_debug(String2, jointdbg),
    get_intention2(Act, Intentions, Intention),
    format(atom(String3), "[JOINT] INTENTION: ~w~n", [Intention]),
    print_debug(String3, jointdbg).



update_model(biggest_joint_reasoning):-
    retractall(model_intention( _ )),
    retractall(model_action( _ )),

    format(atom(String), "[JOINT] UPDATING JOINT MODEL~n", []),
    bagof(intention(Intention_ID, Context, Status),
	  intention(Intention_ID, Context, Status), Intentions),
    !,
    format(atom(String2),
	   "[JOINT] UPDATING JOINT MODEL, INTENTIONS: ~w ~n", [Intentions]),
    get_model_intention(biggest_joint_reasoning, Intentions, Intention),
    format(atom(String3),
	   "[JOINT] UPDATING JOINT MODEL, INTENTION: ~w ~n", [Intention]),
    print_debug(String, jointdbg),
    print_debug(String2, jointdbg),
    print_debug(String3, jointdbg),


    Intention = intention( _,  [ plan(_, _, _, _, _, [Act | _])|  _ ], _),

    assert(model_intention(Intention)),
    assert(model_action(Act)),

    format(atom(String4), "[JOINT] UPDATING JOINT MODEL, ACT: ~w ~n", [Act]),
    print_debug(String, jointdbg),
    print_debug(String2, jointdbg),
    print_debug(String3, jointdbg),
    print_debug(String4, jointdbg).

update_model(biggest_joint_reasoning).



get_intention(biggest_joint_reasoning, Intentions, Intention):-
    get_not_joint_action(Intentions, Intention).

get_intention(biggest_joint_reasoning, _, Intention):-
    model_intention(Intention).



%% K cemu jsou ty dole?

%  get_intention(simple_reasoning,[intention(IDX,CONTENT,active)|_],
%		                   intention(IDX,CONTENT,active)).
%
%  get_intention(simple_reasoning,[_|T],INTENTION):-
%	get_intention(simple_reasoning,T,INTENTION).
%
%

%
%	redirecting the other two to simple_reasoning
%

get_plan(biggest_joint_reasoning, Event, Means, Intended_Means):-
    get_plan(simple_reasoning, Event, Means, Intended_Means).

get_substitution(biggest_joint_reasoning, Action, _, _, Substitution):-
    model_action(Action2),
    format(atom(String),
	   "[JOINT] ACTION ~w MODEL ACTION: ~w ~n", [Action, Action2]),
    print_debug(String, jointdbg),
    unifiable(act(Action), Action2, Substitution),
    format(atom(String2), "[JOINT] SUBSTITUTION: ~w ~n", [Substitution]),
    print_debug(String2, jointdbg).

get_substitution(biggest_joint_reasoning, Action, Context, Vars, Substitution)
    :-
    get_substitution(simple_reasoning, Action, Context, Vars, Substitution).



init_reasoning(biggest_joint_reasoning).

