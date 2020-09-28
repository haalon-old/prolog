insert(E1,[],[E1]).
insert(E1,[E2|T],[E1,E2|T]):-E1<E2,!.
insert(E1,[E2|T],[E2|T1]):-insert(E1,T,T1).

sort2([],[]).
sort2([E|T], T1):-sort2(T,T2), insert(E,T2,T1).