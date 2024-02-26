

:- include('stats.pl').


:- dynamic obrat/3.




pridej(sold(Who, Number), Number_Total):-
    obrat(Who, Number2, Number_Total2),
    retract(obrat(Who, Number2, Number_Total2)),
    Number3 is Number + Number2,
    Number_Total3 is Number_Total + Number_Total2,
    assert(obrat(Who, Number3, Number_Total3)).

pridej(sold(Who, Number), Number_Total):-
    assert(obrat(Who, Number, Number_Total)).

g2([], _, _).

g2([Trade|Trades], B, S):-
    BS is (B+S) div 2,
    pridej(Trade, BS),
    writeln(pridej(Trade, BS)),
    g2(Trades, B, S).


report([]).

report([obrat(Who, N, NT) | Obraty]):-
    P is N * 100 / NT,
    format(" ~w : ~w~n", [Who, P]),
    report(Obraty).


g1([]).

g1([Stat | Stats]):-
    member(buyers(B), Stat),
    member(sellers(S), Stat),
    member(sold(Trades), Stat),
    g2(Trades, B, S),
    g1(Stats).


sumList([], 0).

sumList([H|T], Sum):-
    sumlist(T, Sum2),
    Sum is Sum2 + H.

casy([]).

casy([Agent | Agents]):-
   bagof(Cas, stats(Agent, Cas, _), Casy),
   sumList(Casy, Sum),
   length(Casy, Length),
   Prumer is Sum / Length,
   format("~w prumerne ~w~n",[Agent, Prumer]),
   casy(Agents).

g:-
   bagof(Stat, stats_(Stat), Stats),
   g1(Stats),
   bagof(obrat(Who, N, NT), obrat(Who, N, NT), Obraty),
   report(Obraty),
   findall(Person, stats(Person, _, _), Persons),
   sort(Persons, Persons_S),
   casy(Persons_S).




                     