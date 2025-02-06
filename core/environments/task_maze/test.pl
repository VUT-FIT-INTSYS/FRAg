

pw(Agent):-
    task_maze(perceive, Agent, Add_List, Delete_List),
    write(Agent),write(' add:'),writeln(Add_List),
    write(Agent),write(' delete:'),writeln(Delete_List).
    
g:-
    use_module('task_maze.pl'),
    task_maze(add_agent, franta),
    pw(franta),
    task_maze(act, franta, pick, Result1),
    task_maze(act, franta, go(down), Result2),
    pw(franta),
    task_maze(act, franta, pick, Result3),
    task_maze(act, franta, go(down), Result4),
    pw(franta),
    task_maze(act, franta, pick, Result5),
    task_maze(act, franta, go(down), Result6),
    pw(franta),
    task_maze(act, franta, pick, Result7),
    task_maze(act, franta, go(down), Result8),
    pw(franta),
    task_maze(act, franta, go(down), Result7),
    pw(franta).



/*   
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
 */
