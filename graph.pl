edge(a,b,2).
edge(b,c,2).
edge(c,d,2).
edge(d,e,2).
edge(e,f,2).
edge(f,a,2).

%edge(a,d,3).
edge(b,e,3).
edge(c,f,3).

edge2(X,Y,C):-edge(X,Y,C);edge(Y,X,C).

chain(C):-chain2([],C).
chain2(_,[_]).
chain2(Edges, [V1,V2|VS]):-sort([V1,V2], Edge), not(member(Edge,Edges)),chain2([Edge|Edges], [V2|VS]).

cycle(X,Cycle):-path2(X,X,[],Cycle),dif(Cycle,[X]),chain(Cycle).
path(X,Y,Path):-path2(X,Y,[X],Path).
path2(X,X,_,[X]).
path2(X,Y,Vis, [X|P]):-edge2(X,Z,_),not(member(Z,Vis)),path2(Z,Y,[Z|Vis],P).


pathCost([_],0).
pathCost([A,B|T],Sum):-edge2(A,B,Cost),pathCost([B|T],Sum2), Sum is Sum2 + Cost.

predMin(X,Y,M):- M is min(X,Y).
list_min([L|LS],Min):- foldl(predMin,LS,L,Min).

min_path(X,Y,Path):-
	bagof(L, path(X,Y,L), LL), 
	maplist(pathCost,LL,Costs),
	list_min(Costs, Min),!,
	nth0(Ind,Costs,Min),
	nth0(Ind,LL,Path).

startWith(_,[],[]).
startWith(X, [ [X|XS] | XSS] , [[X|XS]|Rest]):- startWith(X,XSS,Rest),!.
startWith(X, [ _ | XSS] , Rest):- startWith(X,XSS,Rest).

adjacent(X,LS):-bagof(Y,edge2(X,Y,_),LS).
reversedStep(XS,Res):- bagof(R, reversedStep2(XS,R), Res).
reversedStep2([X|XS],[Y,X|XS]):-edge2(X,Y,_).

expandRegion(Old,New):-
	maplist(adjacent,Old,NewNest),
	foldl(append,NewNest,[],NewDup),
	append(Old,NewDup,NewAll),
	list_to_set(NewAll,New).


short_path(X,Y,Path):-short_path2(Y,[X],[[X]],Path).
short_path2(Y,Vis,PS, P):-startWith(Y,PS,Res), dif(Res,[]),!, member(PR,Res), reverse(PR,P).
short_path2(Y,Vis, Paths, P):-
	expandRegion(Vis,VisNew),
	dif(VisNew,Vis), 

	maplist(reversedStep,Paths,NextNested),
	foldl(append,NextNested,[],NextDup),
	include(is_set,NextDup,NextPaths), %remove cycles
	!,
	short_path2(Y, VisNew, NextPaths, P).

getFace(X,Face2):-getFace2([X],Face),sort(Face,Face2).
getFace2(Old,Face):-expandRegion(Old,New),dif(Old,New),getFace2(New,Face),!.
getFace2(Face,Face).

getV(YS):-bagof(Y,edge2(_,Y,_),YS).
getVertices(VS4):- bagof(VS,getV(VS),VSS), foldl(append,VSS,[], VS2),list_to_set(VS2,VS3),sort(VS3,VS4).

cyclic:-getVertices(V), convlist(cycle,V,CS), dif(CS,[]).

is_connected:-getVertices([V|VS]),getFace(V,Face),Face=[V|VS].