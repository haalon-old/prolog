
%% g(L,X):-loop(L,[],X).


%% triplet(L, (A,B,C)):-member(A,L),member(B,L),member(C,L), A<B, C*C =:= B*B + A*A.



%% loop(L, Acc, Res):- triplet(L,T), not(member(T,Acc)), loop(L, [T|Acc], Res).
%% loop(_, Res, Res).

%% number(L,M,L):-L<M.
%% number(L,M,X):-L<M, L2 is L+1, number(L2,M,X).

%% comp(N):- number(2,N,X), number(2,N,Y), N =:= X*Y,!.

%% dist2([X1,Y1],[X2,Y2],D):- D is (X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2),!.


%% notmax(P1,P2,L):- member(P3,L), P3 \== P2, dist2(P1,P2,D1),dist2(P1,P3,D2), D2>D1.
%% maxdist(P, L, M) :- member(M,L), not(notmax(P,M,L)).

%% maxs( [],_, [], _).
%% maxs( [P|L], All, [ [P,M] | Rest], Acc):-not(member(P,Acc)), print(P), maxdist(P, All, M), print(M), nl, maxs(L, All, Rest, [M | Acc]),!.
%% maxs( [_|L], All,  Rest, Acc):- maxs(L, All, Rest, Acc).


%% maxs( [],_, []).
%% maxs( [P|L], All, [ [P,M] | Rest]):-  maxdist(P, All, M),  maxs(L, All, Rest).


%% g(L,X):- maxs(L,L,X,[]).

%% mix(U,V,W):- split(U,U1,U2), split(V,V1,V2), append([U1,V1,U2,V2], W).

%% split([E1 | L], [E1], L):- L \= [].
%% split( [E1 | L], [E1|H], T ):-split(L, H, T).



hasdelim(E,ES):- member(Y, ES), Y mod E =:= 0,!.
hasdelim(E,ES):- member(Y, ES), E mod Y =:= 0,!.

longer([_|LS], [_|KS]):-longer(LS,KS).
longer([_|_], []).

neq(L1,L2):-member(E1,L1), not( member(E1, L2)),!.
neq(L1,L2):-member(E2,L2), not( member(E2, L1)),!.

primeset(All, Acc, Res):- member(A,All), not(member(A,Acc)), not( hasdelim(A,Acc)), primeset(All, [A|Acc], Res).
primeset(_ , Res, Res).

nmax_prime_len(All,ES):- primeset(All, [], AS), neq(ES, AS), longer(AS,ES),!.

g(L,X):- primeset(L,[],X), not( nmax_prime_len(L,X)).