delete_one(E, [E|T], T).
delete_one(E, [X|T], [X|T1]):-delete_one(E, T, T1).

delete_before(E, [E|T], T).
delete_before(E, [X|T], T1):-delete_before(E, T, T1).

subsetII([], YS).
subsetII([X|XS], YS):-member(X,YS), delete_one(X,YS, YS2), subsetII(XS,YS2).

subsetOI([], YS).
subsetOI([X|XS], YS):-member(X,YS), delete_before(X,YS, YS2), subsetOI(XS,YS2).

