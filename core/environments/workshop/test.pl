

pw(Agent):-
    workshop(perceive, Agent, Add_List, Delete_List),
    write(Agent),write(' add:'),writeln(Add_List),
    write(Agent),write(' delete:'),writeln(Delete_List).
   

g3(0).


g3(N):-
    pw(adam),
    N2 is N-1,
    g3(N2).

g2(N):-
    use_module('workshop.pl'),
    card_shop(add_agent, adam),
    g3(N).

g:-
    use_module('workshop.pl'),
    workshop(add_agent, adam),
    pw(adam).
 /*   card_shop(act, adam, sell(hana, cd5), Result1),
    card_shop(act, vera, sell(adam, vera, cd3), _),
    card_shop(act, vera, sell(adam, hana, cd8), _),
    card_shop(act, vera, sell(adam, hana, cd1), _),
    card_shop(remove_environment_clone, kartovy_klon),
    card_shop(act, adam, sell(adam, vera, cd8), _),
    pw(vera),
    pw(adam),
    pw(hana).
 */   

