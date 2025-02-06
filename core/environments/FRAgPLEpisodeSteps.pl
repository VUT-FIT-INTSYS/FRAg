
:-module(episode_steps, 
    [
        add_agent / 3,
        is_new_episode / 0
     ]
).
                       

% List of agents that perceived in an instance of Environment
% [perceived(Agent, Environment, Instance)]
:-dynamic agents_perceived / 1.
:-dynamic history /1.
:-dynamic new_episode /0.



agents_perceived([]).
history([]).

add_agent(Agent, Environment, Instance):-
    agents_perceived(List),
    history(List2),
    member(perceived(Agent, Environment, Instance), List),
    assert(new_episode),
    retractall(history(List2)),
    assert(history([new_episode, perceived(Agent, Environment, Instance) | List2])), 
    retractall(agents_perceived(List)),
    assert(agents_perceived([perceived(Agent, Environment, Instance)])).


add_agent(Agent, Environment, Instance):-
    agents_perceived(List),
    history(List2),
    retractall(agents_perceived(List)),
    retractall(history(List2)),
    assert(history([perceived(Agent, Environment, Instance) | List])),
    assert(agents_perceived([perceived(Agent, Environment, Instance) | List])).

is_new_episode:-
    new_episode,
    retract(new_episode).
