


is_debug(Debug, true):-
    agent_debug(Debug).

is_debug(_, false).



print_debug(Content, Debug):-
    agent_debug(Debug),
    extend_debug_content(Content, Debug, Debug_Content),
    write(Debug_Content).

print_debug(_, _).


 % 'format' print / no sense fot new line version

print_debug(Content, Data, Debug):-
    agent_debug(Debug),
    extend_debug_content(Content, Debug, Debug_Content),
    format(Debug_Content, Data).

print_debug(_, _, _).

println_debug(Content, Debug):-
    agent_debug(Debug),
    !,
    extend_debug_content(Content, Debug, Debug_Content),
    write(Debug_Content),
    nl.

println_debug(_, _).

extend_debug_content(Content, Debug, Debug_Content):-
    agent_debug_caption(Debug, Caption),
    concat(Caption, Content, Debug_Content).

agent_debug_caption(interdbg, "[INTER] ").
agent_debug_caption(systemdbg, "[SYSTEM] ").
agent_debug_caption(reasoningdbg, "[REASON] ").
agent_debug_caption(mctsdbg, "[MCTS] ").
agent_debug_caption(actdbg, "[ACT] ").
agent_debug_caption(error, "[ERROR] ").
agent_debug_caption(mctsdbg_path, "[MCPTH]").
agent_debug_caption( _, "[OTHERS] ").


print_list_debug([], _).

print_list_debug([H|T], Debug):-
    term_string(H, HS),
    concat(" * ", HS, HS2),
    println_debug(HS2, Debug),
    print_list_debug(T, Debug). 


print_list_state([],S,S).

print_list_state([H|T], S, String_Out):-
    term_string(H, HS),
    concat(S, HS, S2),
    concat(S2, ";\n", S3),
    print_list_state(T, S3, String_Out).



print_plans([], String, String).

print_plans([plan(Plan_ID, Event_Type, Event_Atom, Conditions, Context, Body)|
             Plans], String_In, String_Out):-
    format(atom(String1), "   plan(~w, ~w, ~w, ~w ~w ~n       ~w)~n ",
           [Plan_ID, Event_Type, Event_Atom, Conditions, Context, Body]),
    concat(String_In, String1, String2),
    print_plans(Plans, String2, String_Out).



print_intention([], String, String).

print_intention([intention(Intention_ID, Plan_Stack, Status)| Intentions],
                String_In, String_Out):-
    concat(String_In, "intention:", String1),
    concat(String1, Intention_ID, String2),
    concat(String2, "\n", String3),
    print_plans(Plan_Stack, String3, String4),
    term_string(Status, String5),
    concat(String4, String5 , String6),
    concat(String6, "\n", String7),
    print_intention(Intentions, String7, String_Out).



print_intentions(String_In, String_Out):-
    bagof(intention(Intention_ID, Plan_Stack, Status),
          intention(Intention_ID, Plan_Stack, Status), Intentions),
    concat(String_In, ":: INTENTIONS {\n", String1),
    % print_list_state(INTENTIONS, STRING2, STRINGINTENTIONS),
    print_intention(Intentions, String1, String2),
    concat(String2, "}\n", String_Out).



print_intentions(String, String_Intention):-
    concat(String,":: INTENTIONS: No intentions\n",String_Intention).



print_goals(String, String_Goal):-
    bagof(event(Event_ID, Type, Atom, Parent_Intention, Context, Status,
                History),
          event(Event_ID, Type, Atom, Parent_Intention, Context, Status,
                History),
          Events),
    concat(String,":: EVENTS {\n", String2),
    print_list_state(Events, String2, String3),
    concat(String3, "}\n", String_Goal).

print_goals(String, String_Goal):-
    concat(String, ":: EVENTS: No events\n", String_Goal).



print_beliefs(String_In ,String_Beliefs):-
    bagof(fact(Belief), fact(Belief), Beliefs),
    concat(String_In,":: BELIEFS {\n", String1),
    print_list_state(Beliefs, String1, String_Facts),
    concat(String_Facts, "}\n", String_Beliefs).

print_beliefs(Label, Belief):-
    concat(Label, ":: BELIEFS: No beliefs\n", Belief).



print_agent_state(Debug):-
    loop_number(Loop),
    thread_self(Agent),
    format(atom(String1),"~n:: vvvvvvvvvvvvvvvvvvvvvvvvvv~n",[]),
    format(atom(String2),":: Name:~w~n", [Agent]),
    format(atom(String3),":: LOOP ~w~n", [Loop]),
    concat(String1, String2, String4),
    concat(String4 ,String3, String5),
    print_intentions(String5, String6),
    print_goals(String6, String7),
    print_beliefs(String7, String8),
    format(atom(String9),":: ^^^^^^^^^^^^^^^^^^^^^^^^^^~n",[]),
    concat(String8, String9, String10),
    print_debug(String10, Debug).



print_state( _ ):-
    agent_debug(no_debug).

print_state(Message):-
%    println_debug('', reasoningdbg),
    println_debug(Message, reasoningdbg),
    print_agent_state(reasoningdbg).

print_state(_).


