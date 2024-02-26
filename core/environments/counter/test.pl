

pw(Agent):-
    simple_counter(perceive, Agent, Add_List, Delete_List),
    write(Agent),write(' add:'),writeln(Add_List),
    write(Agent),write(' delete:'),writeln(Delete_List).
    
g:-
    use_module('counter.pl'),
    simple_counter(add_agent, franta),
    pw(franta),
    simple_counter(act, franta, increment, Result1),
    simple_counter(clone, klon),
    simple_counter(clone, klon2),
    simple_counter(add_agent, pepa, klon),
    simple_counter(act, pepa, decrement, Result2),
    pw(franta),
    pw(pepa),
    simple_counter(act, pepa, decrement, Result3),
    simple_counter(act, pepa, decrement, Result4),
    simple_counter(act, pepa, decrement, Result5),
    simple_counter(add_agent, honza, klon),
    pw(honza),
    simple_counter(act, pepa, decrement, Result6),
    simple_counter(act, franta, increment, Result7),
    simple_counter(act, franta, increment, Result8),
    simple_counter(act, franta, increment, Result9),
    simple_counter(add_agent, pepa, klon2), 
    simple_counter(remove_environment_clone, klon),
    simple_counter(act, honza, decrement, Result10),
    pw(pepa),
    pw(honza),
    pw(franta),
    pw(hana).

