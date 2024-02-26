
:-module(stat_utils, 
    [
        normal_dist_sample /3,
        get_discount / 3,
        new_events_number / 2
     ]
).


factorial(0, 1).
factorial(N, F) :- 
    N > 0, 
    M is N - 1, 
    factorial(M,T), 
    F is N * T.

poisson(Lambda, X,Y):-
    Lambda>100,
    poisson(100, X, Y).

poisson(Lambda, X, Y):-
%  (Lambda^X * exp(-Lambda)) / X!
    factorial(X, XF),
    LX is Lambda ** X,
    EL is exp(-Lambda),
    Y is (LX*EL) / XF.
  

new_events_number2(Events, _, S, X, Events2):-
    S>X,
    Events2 is Events -1.

new_events_number2(N, Lambda, S, X, Events):-
    poisson(Lambda, N, X2),
    S2 is S+X2,
    N2 is N+1,
    new_events_number2(N2, Lambda, S2, X, Events).
                     
new_events_number(Lambda, Events):-
    random(0.0, 1.0, X),
    new_events_number2(0, Lambda, 0, X, Events).


f_normal(X, Mean, Dispersion, Y):-
    Y is (1 / (Dispersion*2.507)) * exp(-0.5*((X- Mean) / Dispersion)**2).



normal_dist_sample2( _, _, Y, Y2, X, X):-
    Y < Y2.

normal_dist_sample2(Mean, Dispersion, _, _, _, X_Out):-
    normal_dist_sample(Mean, Dispersion, X_Out).


normal_dist_sample(Mean, Dispersion, X_Out):-
   f_normal(Mean, Mean, Dispersion, Y_Max),
   random(0.0, 1.0, X),
   random(0.0, Y_Max, Y),
   f_normal(X, Mean, Dispersion, Y2),
   normal_dist_sample2(Mean, Dispersion, Y, Y2, X, X_Out).


get_discount(Mean, Dispersion, Discount):-
    normal_dist_sample(Mean, Dispersion, Discount).



g(0).

g(X):-
%   normal_dist_sample(0.6, 0.1, Y),
new_events_number(1.8, _ , Y),
   writeln(Y),
   X2 is X-1,
   g(X2).




g(_, 0).

g(L, X):-
    new_event_number(L, N),
    writeln(N),
    X2 is X-1,
    g(L, X2).


