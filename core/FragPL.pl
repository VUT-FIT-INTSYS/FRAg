
%
%
%		#################    ###############           #######
%		#################    #################       ###########
%		###                  ###           ###     ###         ###
%		###                  ###           ###    ###           ###
%		###                  ###           ###    ###           ###
%		############         ################     #################       #############
%		############         #############        #################      #################
%		###                  ###       ###        ###           ###     ###             ###
%		###                  ###        ###       ###           ###     ###             ###
%		###                  ###         ###      ###           ###     ###             ###
%		###                  ###          ###     ###           ###      #################
%		###                  ###           ###    ###           ###       ################
%                                                                                               ###
%                                                                                ###           ###
%                                                                                 ###############
%						                                    ############
%
%



:-module(fRAg,
    [
        set_bindings /1,
	set_default_reasoning /2,
	get_frag_attributes /2,
	frag /0,
	frag /1
    ]
).



/** <module>  FRAg: The Flexible Reasoning Agent

Main module of the FRAg system.

@author Frantisek Zboril jr.
@license GPL

*/


:- use_module('FRAgSync').
:- use_module('FRAgAgent').

:- discontiguous frag_choice/1.
:- discontiguous frag_choice/2.

version("0.95").



%!  set_bindings(+Binding_Method) is det
%   Sets variable bindings method / strategy
%  @arg Binding_Method: one from 'late', 'early'

set_bindings(late):-
    fRAgAgent:set_late_bindings.

set_bindings(early):-
    fRAgAgent:set_early_bindings.



%!  get_frag_attributes(+Key, -Value) is det
%   Returns actual values of attributes
%  @arg Key: attribute name, @see documentation
%  @arg Value: attribute value

get_frag_attributes(default_bindings, late):-
    fRAgAgent:is_default_late_bindings.

get_frag_attributes(default_bindings, early).

get_frag_attributes(bindings, late):-
    fRAgAgent:is_late_bindings.

get_frag_attributes(bindings, early).

get_frag_attributes(reasonings, [Intention_Selection, Plan_Selection,
                                 Substitution_Selection]):-
    fRAgAgent:get_default_reasoning(Intention_Selection, Plan_Selection,
                                    Substitution_Selection).

get_frag_attributes(debugs, Debugs):-
    findall(Debug, frag_debug(Debug), Debugs).

get_frag_attributes(environments, Environments):-
    get_default_environments(Environments).



%!  set_default_reasoning(+reasoning, +reasoning_method) is det
%   Sets reasoning method for intention, plan or substitution selection. Could 
%   set all of them at once.
%  @arg Reasoning: intention_selection, plan_selection, substitution_selection, all/
%  @arg Reasoning_method: one of possible reasoning or a list of three in 
%   the case of 'all'

set_default_reasoning(intention_selection, Intention_Selection):-
    set_default_intention_selection(Intention_Selection).

set_default_reasoning(plan_selection, Plan_Selection):-
    set_default_plan_selection(Plan_Selection).

set_default_reasoning(substitution_selection, Substitution_Selection):-
    set_default_substitution_selection(Substitution_Selection).

set_default_reasoning(all, Reasoning):-
    set_default_reasoning(Reasoning).

               

%!  load_agent(+Agent, +Program, +Attributes, -Thread) is det
%   Creates thread for Agent, loads Agent's Program  and sets it up
%  @arg Agent: agent name
%  @arg Program: agent program - 'fap' program file
%  @arg Attributes: agent attributes @see @tbd
%  @arg Thread: created thread for the agent

load_agent(Agent, Program, Attributes, Thread):-
    term_string(Agent_Term, Agent),
    fa_sync:sync_add_agent(Agent_Term),
    thread_create(fa_init_agent(Program, Attributes), Thread,
                  [alias(Agent_Term)]),
    assert(agent(Agent_Term)).



%!  load_same_agents(+Agent, +Program, +Number, +Attributes, -Threads) is nondet
%  @arg Agent: agent name prefix (names will be Agent1, Agent2 ... AgentNumber)
%  @arg Program: agent program - 'fap' program file
%  @arg Number: number of agents to be created
%  @arg Attributes: attributes declared for these agents
%  @arg Threads: created Threads for the agents

load_same_agents(_, _, 0, _, []).

load_same_agents(Agent, Program, Number, Attributes, [Thread| THT]):-
    concat(Agent, Number, AGENTNAME),
    load_agent(AGENTNAME, Program, Attributes, Thread),
    Number2 is Number - 1,
    load_same_agents(Agent, Program, Number2, Attributes, THT).



%!  load_agents(Agent_To_Load, Agent_Threads) is det
%   Loads agents of given name (with possible indexes), type (Program) and
%   sets Attributes for each agent type
%  @arg Agent_To_Load: load(Agent, Program, Number, Attributes) where Agent is
%   agent 'root' name, Number is number of agents that should be created from 
%   given Program - then the names contains indexes from 1 to Number, and 
%   Attributes are attribudes specified for the agent type
%  @Agent_Threads: List of threads of each created agent

load_agents([],[]).

load_agents([load(Agent, Program, 1, Attributes)| Agents],
            [Agent_Thread| Agent_Threads]):-
    load_agent(Agent, Program, Attributes, Agent_Thread),
    load_agents(Agents, Agent_Threads).

load_agents([load(Agent, Program, Number, Attributes)| Agents],
            Agent_Threads):-
    % posledni term je seznam vytvorenych vlaken
    load_same_agents(Agent, Program, Number, Attributes, Agent_Threads1),
    load_agents(Agents, Agent_Threads2),
    append(Agent_Threads1, Agent_Threads2, Agent_Threads).



%!  frag_process_attributes(+List_Of_Attributes)
%   Process default attributes of the system
%  @arg List_Of_Attributes: attribute is a tuple (Key, Value)
%   Possible attributes are in documentation @see @tbd

frag_process_attributes([]).

frag_process_attributes([(Key, Value)| Attributes]):-
    set_default_attribute(Key, Value),
    frag_process_attributes(Attributes).



%!  set_default_attribute(+Key, +Value) is det
%   Sets attributes of the multiagent system. These settings are default
%   to agents and can be changed for individual agents in the agent
%   declaration clauses @see multiagent frag file
%  @arg Key: attribute name
%  @arg Value: attribute value
%  @see documentation for the mas2fp file format

set_default_attribute(control, Control):-
    fRAgAgent:set_control(Control).

set_default_attribute(reasoning, Reasoning):-
    fRAgAgent:set_default_reasoning(Reasoning).

set_default_attribute(bindings ,late):-
    fRAgAgent:set_default_late_bindings(true).

set_default_attribute(bindings ,early):-
    fRAgAgent:set_default_late_bindings(false).

set_default_attribute(reasoning_params , Parameters):-
    fRAgAgent:set_reasoning_params(Parameters).

set_default_attribute(environment, Environment):-
    fRAgAgent:set_default_environment(Environment).



%!  frag(+Filename) is det
%   Loads and performs multiagent system declared in file Filename.
%  @arg Filename: multiagent file for the FRAg system
%  @see documentation for the mas2fp file format

frag(Filename):-
    format(atom(Mas2FP),"~w.mas2fp",[Filename]),
    current_module(fRAg, FRAg_Path),
    absolute_file_name(Mas2FP, Absolute_Mas2FP, [relative_to(FRAg_Path)]),
    access_file(Absolute_Mas2FP, read),
    !,
    open(Absolute_Mas2FP, read, Stream, [close_on_abort(true)]),
    thread_setconcurrency(_ , 1000),
    load_multiagent(Stream, Agents),
    !,
    close(Stream),
    fa_sync:sync_init,
    load_agents(Agents, Threads),
    fa_sync:sync_agents_ready,
    !,
%    wait_agents(Threads),
%    run (unblock) agents
%    assert(go(1)),
%    wait_agents(Threads),
    join_threads(Threads).

frag(Filename):-
    format("[MAS2FP] Metafile ~w.mas2fp does not exists.~n", [Filename]).



%!  load_multiagent(+Stream, -Agents_To_Load) is det TODO
%   Process mas2fp metafile -> creates multiagent infrastructure
%  @arg Stream: Stream of the metafile
%  @arg Agents_To_Load: List of agents that have to be loaded into the system

load_multiagent(Stream, Agents_To_Load):-
    read_clause(Stream, Clause, []),
    !,
    frag_process_clause(Stream, Clause, Agents_To_Load).

load_multiagent(_, []).



%!  frag_process_claues(+Stream, +Clause, +Agents_To_Load) is det
%   Process 'mas2fp' metafile. Possible clauses are commented below.
%  @arg Stream: Metafile stream
%  @arg Clause: Actual clause to process
%  @arg Agents_To_Load: List of agent, that are required in the metafile

%  no more clauses in Stream

frag_process_clause(_ , end_of_file, []):-
    !.

%  sets environment's attribute

frag_process_clause(Stream, set_environment(Environment, Parameters), Clauses):-
    fRAgAgent:set_environment_parameters(Environment, Parameters),
    !,
    load_multiagent(Stream, Clauses).

%  sets default attributes

frag_process_clause(Stream, set_agents(Attributes), Clauses):-
    frag_process_attributes(Attributes),
    !,
    load_multiagent(Stream, Clauses).

%  include reasoning method, possible reasoning method in @todo documentation

frag_process_clause(Stream, include_reasoning(Filename), Clauses):-
    fRAgAgent:include_reasoning_method(Filename),
    !,
    load_multiagent(Stream, Clauses).

%  include multiagent system environment, some provided environments in actual 
%  distribution are described in @todo documentation

frag_process_clause(Stream, include_environment(Filename), Clauses):-
    fRAgAgent:load_environment(Filename),
    !,
    load_multiagent(Stream, Clauses).

%  loads a new agent in some number and attributes

frag_process_clause(Stream, load(Filename, Agent, Number, Attributes),
		    [load(Filename, Agent, Number, Attributes)| Clauses])
    :-
    !,
    load_multiagent(Stream, Clauses).

%  loads / consults arbitrary Prolog file

frag_process_clause(Stream, load(Filename), Clauses):-
    consult(Filename),
    !,
    load_multiagent(Stream, Clauses).

%  unknown metafile clause -> error

frag_process_clause(Stream, Clause, Clauses):-
    format("[MAS2FP] Error processing clause ~w~n", [Clause]),
    !,
    load_multiagent(Stream, Clauses).



%!  wait_agents(+Threads) is det
%   Barrier with active waiting. All agents have to report their readiness by 
%   inserting the ready(Agent_Name) atom
%  @arg Threads: List of agents threads, these threads signalizes 
%   ready(Agent_Name) when they are ready 

wait_agents([]).		% no agents loaded

wait_agents(Threads):-
    bagof(Agent, fa_sync:ready(Agent), Agents_Ready),
    length(Agents_Ready, Agents_Ready_Length),
    length(Threads, Agents_Ready_Length),
    retractall(ready( _ )).

wait_agents(Threads):-
    wait_agents(Threads).



%!  frag is det
%   Starts the system as a console with menu

frag:-
    writeln("Select your choice"),
    writeln("1, run program"),
    writeln("2, set reasoning"),
    writeln("3, set bindings"),
    writeln("4, set debugs"),
    writeln("5, set gspy loop"),
    writeln("6, print settings"),
    writeln("0, exit"),
    get_single_char(Choice),
    writeln(Choice),
    frag_choice(Choice),
    writeln(bye).



%===============================================================================
%                                                                              |
%    CONSOLE                                                                   |
%                                                                              |
%===============================================================================

frag_choice(48).

frag_choice(49):-
    writeln("program name:"),
    read(File),
    frag(File).

frag_choice(50,49):-
    set_default_reasoning(all, mcts_reasoning),
    frag.

frag_choice(50,50):-
    set_default_reasoning(all, simple_reasoning),
    frag.

frag_choice(50,51):-
    set_default_reasoning(all, random_reasoning),
    frag.

frag_choice(50,52):-
    set_default_reasoning(intention_selection, biggest_joint_reasoning),
    frag.

frag_choice(50,53):-
    set_default_reasoning(plan_selection, robin_reasoning),
    frag.

frag_choice(50):-
    get_default_reasoning(Intention_Reasoning, Plan_Reasoning,
                          Substitution_Reasoning),
    writeln([Intention_Reasoning, Plan_Reasoning, Substitution_Reasoning]),
    writeln("1, mcts reasoning"),
    writeln("2, simple reasoning"),
    writeln("3, random reasoning"),
    writeln("4, biggest joint (for intention selection only)"),
    writeln("5, round robin reasoning (for plan selection only)"),
    get_single_char(Choince),
    frag_choice(50, Choince).

frag_choice(51,49):-
    set_bindings(late),
    frag.

frag_choice(51,50):-
    set_bindings(early),
    frag.

frag_choice(51):-
    writeln("1, late bindings"),
    writeln("2, early bindings"),
    get_single_char(Choice),
    frag_choice(51, Choice).

frag_choice(52):-
    writeln("todo"),
    frag.

frag_choice(53):-
    gspy(loop),
    frag.

frag_choice(54):-
    get_frag_attributes(default_bindings, Bindings),
    get_frag_attributes(reasonings, [Intention_Selection, Plan_Selection,
                                     Substitution_Selection]),
    get_frag_attributes(debugs, Debugs),
    nl,
    writeln("Settings"),
    writeln("--------"),
    writeln("Default bindings:"),
    write(".. "),write(Bindings),nl,
    writeln("Reasonings:"),
    write("-> Intention selection: "),write(Intention_Selection),nl,
    write("-> Plan selection: "),write(Plan_Selection),nl,
    write("-> Substitution selection: "),write(Substitution_Selection),nl,
    write("-> Debugs:"), write(Debugs), nl,
    nl,
    frag.

frag_choice( _ ):-
    frag.



%!  join_threads(+Threads) is det
%   Joins Threads (agents) to the main thread. These agents have finished. 
%  @arg Threads: List of threads to join the main thread

join_threads([]).

join_threads([Thread| Threads]):-
    thread_join(Thread, _),    % Prolog native
    join_threads(Threads).



%!  main_frag is det
%   Initializes the FRAg system. Initialization parameters is in
%   multiagent file @see fraginit.mas2fp. Then prints initial
%   information about the system, including settings of essential
%   parameters

main_frag:-
    set_prolog_stack(global, limit(8 000 000 000)),
    set_prolog_stack(trail, limit(5 000 000 000)),
    set_prolog_stack(local, limit(5 000 000 000)),
		
    nl,
    version(Version),
    format(
'FRAg version ~w, 2021 - 2024, by Frantisek Zboril & Frantisek Vidensky,
Brno University of Technology ~n~n',
	   [Version]),
    frag('fraginit'),
    !,
    get_frag_attributes(default_bindings, Bindings),
    get_frag_attributes(reasonings, [Intention_Selection, Plan_Selection,
	                Substitution_selection]),
    get_default_environments(Environments),
    format('-> Bindings: ~w~n-> Intention selection: ~w ~n-> Plan selection: ~w
-> Substitution selection: ~w ~n-> Environments: ~w ~n~n',
	   [Bindings, Intention_Selection, Plan_Selection,
            Substitution_selection, Environments]).



:-initialization(main_frag, after_load).



make_documentation:-
    use_module(library(pldoc/doc_library)),
    doc_save('FragPL.pl',[format(html), recursive(true), doc_root(doc)]).


        
