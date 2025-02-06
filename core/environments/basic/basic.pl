 
/**

FragPL, basic environment / ... internal actions
       
@author Frantisek Zboril jr. 
@version 2021 - 2023
@licence
*/

:-dynamic reward /3.


% environment(basic).			% zde by mely byt standardni akce, komunikace ...

% 	me(agent_name)

action(basic, me, 1).

% 	population(list_of_agents)

action(basic, population, 1).

action(basic, reward, 1).

% 	
%   	FRAg internal actions
%   	

action(basic, reward, 1).
action(basic, send,2).
action(basic, bcast,1). 
action(basic, sendfg,2). 		% sendFrag ?
action(basic, printfg,1).
action(basic, printfg,2).
action(basic, foo,1).		% pro ladeni decision, k nicemu
action(basic, foo,2).		% pro ladeni decision, k nicemu
action(basic, foo,3).		% pro ladeni decision, k nicemu
action(basic, foo,4).		% pro ladeni decision, k nicemu

                
% foo action (= printfg)
  % joint_action(Action_symbol, Arity)

joint_action(basic, silently_, 1).   
joint_action(basic, do, 1).
joint_action(basic, jprintfg, 1).


%
%		Particular actions (defined at the begining of this file)                
%



population(Population):-
    bagof(Agent, fRAgBlackboard:agent(Agent), Population).


concatTerm(String1, String2, String):-
    string(String2),
    concat(String1, String2, String).


% S2 is not a String
concatTerm(String1, String2,String):-
    term_string(String2, String2_S),
    concat(String1, String2_S , String).


printfg(String):-    
    me(Agent),
    concatTerm("+: ~w says > ", String, String2),
    concat(String2, "\n", String3),
    format(atom(String4), String3, [Agent]),
    write(current_output, String4).

printfg(String, Parameters):-
	format(atom(String2), String, Parameters),
	term_string(String2, String3),
	printfg(String3).



foo(A):-
    format(atom(String),"ugh ~w",[A]),
    printfg(String).

foo(A,B):-
    format(atom(String),"ugh ~w ~w",[A,B]),
    printfg(String).

foo(A,B,C):-
    format(atom(String),"ugh ~w ~w ~w",[A,B,C]),
    printfg(String).

foo(A,B,C,D):-
    format(atom(String),"ugh ~w ~w ~w ~w",[A,B,C,D]),
    printfg(String).


silently_(_).

me(X):-
    thread_self(X).



sendfg(Receiver, Payload):-
    thread_self(Me),
    printfg("Sending message ~w~n",[Receiver]),
    thread_send_message(Receiver,message(Me, inform,pld(Payload))),
    printfg("Send succeed ~n").

sendfg(_,_):-
    printfg("Send Failed ~w~n").


send(Receiver, Payload):-
    thread_self(Me),
    thread_send_message(Receiver, message(Me, inform, pld(Payload))).


bcast2([], _).
           
bcast2([Agent| Agents], Payload):-
    sendfg(Agent, Payload),
    bcast2(Agents, Payload).


bcast(Payload):-
    bagof(Agent, fRAgBlackboard:agent(Agent) , Agents),
    bcast2(Agents, Payload).
		
bcast( _ ).


%
% 	Joint actions
%

do(X):-
    printfg(X).


    % joint version of printfg
jprintfg(String):-
    printfg(String).	


% basic(act, _, Act, true):-
%    is_exclusive_action(basic, Act),
%    Act.

% reward act must produce Reward as output

basic(act, _, reward(Reward), reward(Reward)).

% silent acts

basic(act, _, silently_(printfg( _ )), true).

basic(act, _, silently_(printfg( _, _)), true).

basic(act, _, silently_(jprintfg( _ )), true).

basic(act, _, silently_(format( _)), true).

basic(act, _, silently_(format( _, _)), true).

basic(act, Agent, silently_(Act), Result):-
% there is none, do we need to silent it?
    basic(act, Agent, Act, Result). 


%   any joint action

basic(act, _, Act, Result):-
    is_joint_action(basic, Act),
    Act,
    Result is Act.

%   any non-joint action

basic(act, _, Act, true):-
    Act.
	         
%   action failed

basic(act, _, _, fail).


