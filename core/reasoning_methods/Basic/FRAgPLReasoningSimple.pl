%
% Reasoning methods - the simple ones
% Frantisek Zboril jr. 2022
%

%
% Should define
%   get_intention(+Reasoning_type, +Intentions, -Intention).
%   get_substitution(+Reasoning_type, +ActionTerm, +SubstitutionList, 
%                    +VariableList,-SubstitutionList).
%   get_plan(+Reasoning_type, +Event, +RelAppPlans, -IntendedMeans).
%

% This module is loaded / included in the FRAgAgent file



  reasoning_method(simple_reasoning).


% First active

%
%  Takes the first active intention
%

% sort it by id

get_intention(simple_reasoning, [intention(Index, Content, active)|_], 
              intention(Index, Content, active)).

get_intention(simple_reasoning, [ _ | Intentions], Intention):-
    get_intention(simple_reasoning, Intentions, Intention).


get_substitution(simple_reasoning, _, [Context| _ ], Variables, Context_Out):-
    shorting(Context, Variables, Context_Out). % from file FRAgPLFRAg

% sort it by id
get_plan(simple_reasoning, _, [Intended_Means| _ ], Intended_Means).

update_model(simple_reasoning).
				
init_reasoning(simple_reasoning).

