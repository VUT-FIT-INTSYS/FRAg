%
% Reasoning methods - Random selection
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
% Selects allways randomly


                     
reasoning_method(random_reasoning).


get_intention(random_reasoning, Intentions, intention(Intention_ID, CONTENT, active)):-
    random_member(intention(Intention_ID, CONTENT, active), Intentions).

get_intention(random_reasoning,INTENTIONS, _):-
    get_intention(random_reasoning,INTENTIONS).


get_substitution(random_reasoning, _, Context, Vars, Context2):-
    random_member(Substitution, Context),
    shorting(Substitution, Vars, Context2).	% from file FRAgPLFRAg

 
get_plan(random_reasoning, _ , MEANS, INTENDEDMEANS):-
    random_member(INTENDEDMEANS, MEANS).


update_model(random_reasoning). 


init_reasoning(random_reasoning).
