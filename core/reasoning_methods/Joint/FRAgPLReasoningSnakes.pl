%
% Reasoning methods - the simple ones
% Frantisek Zboril jr. 2023
%

%
% Should define
%    get_intention(+Reasoning_type,+Intentions,-Intention).
%                       
% not defined for:
%    get_substitution(+Reasoning_type, +ActionTerm, +Substitution_List, 
%                     +Variable_List,-Substitution_List).
%    get_plan(+Reasoning_type, +Event, +RelAppPlans, -IntendedMeans).
%		
% if required, 'simple_reasoning' will be used.
%

%	This module is loaded / included in the FRAgAgent file


reasoning_method(snakes_reasoning).

			
%
%	redirecting the other two to simple_reasoning
%

get_intention(biggest_joint_reasoning, Intentions, Intention).

%
%	TODO
%


%
%	redirecting the other two to simple_reasoning
%


get_substitution(snakes_reasoning, Action, Context, Variables, Substitution):-
    get_substitution(simple_reasoning, Action, Context, Variables, 
                     Substitution).

get_plan(biggest_joit_reasoning, Event, Plans, Intended_Means):-
    get_plan(snakes_reasoning, Event, Plans, Intended_Means).

	
  init_reasoning(snakes_reasoning).



