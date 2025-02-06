
/**

This file is part of the FRAg program. It is included into agent's file 
FRAgAgent.pl. It contains clauses that are applied to strategies for selecting 
intentions, plans and substitutions. Simple reasoning always selects the first 
option from a set of provided options.

@author Frantisek Zboril
@version 2021 - 2022
@license GPL

*/


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


%!  get_plan(simple_reasoning, +Event, +Means, -Intended_Means) is det
%   From the listed means for the Event takes the first one
%  @arg Event:
%  @arg Means:
%  @arg Intended_Means:

get_plan(simple_reasoning, _, [Intended_Means| _ ], Intended_Means).



%!  update_model(simple_reasoning) is det
%   No update is needed. This clause is valid by default  

update_model(simple_reasoning).


		
%!  init_reasoning(simple_reasoning) is det
%   No initialization is needed. This clause is valid by default
		
init_reasoning(simple_reasoning).

