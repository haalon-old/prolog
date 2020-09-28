:- style_check(-discontiguous).
:- dynamic subclass/2.
:- dynamic attribute/3.
:- dynamic object/2.
%% attribute( name, value, class/object)
%% subclass(child, parent)

%% base classes
subclass("landcraft", "transport").
subclass("watercraft", "transport").
subclass("aircraft", "transport").

subclass("human-powered", "transport").
subclass("animal-powered", "transport").
subclass("motorised", "transport").
subclass("unpowered", "transport").

subclass("cargo", "transport").
subclass("public", "transport").
subclass("private", "transport").
subclass("military", "transport").

attribute("service", "cargo", "cargo").
attribute("service", "public", "public").
attribute("service", "private", "private").
attribute("service", "military", "military").


attribute("domain", "water", "watercraft").
attribute("domain", "air", "aircraft").
attribute("domain", "land", "landcraft").

attribute("mover", "human", "human-powered").
attribute("mover", "animal", "animal-powered").
attribute("mover", "engine", "motorised").
attribute("mover", "environment", "unpowered").

%% classes by fuel type
subclass("electric vehicle", "motorised").

attribute("engine", "electric motor", "electric vehicle").
attribute("fuel", "electricity", "electric vehicle").

subclass("diesel vehicle", "motorised").

attribute("fuel", "diesel", "diesel vehicle").
attribute("engine", "diesel engine", "diesel vehicle").

subclass("petrol vehicle", "motorised").

attribute("fuel", "petrol", "petrol vehicle").
attribute("engine", "petrol engine", "petrol vehicle").

subclass("nuclear vehicle", "motorised").

attribute("fuel", "nuclear", "nuclear vehicle").
attribute("engine", "reactor", "nuclear vehicle").

subclass("hybrid diesel electric", "diesel vehicle").
subclass("hybrid diesel electric", "electric vehicle").

subclass("hybrid petrol electric", "petrol vehicle").
subclass("hybrid petrol electric", "electric vehicle").

%% air transport classes
subclass("aerostat", "aircraft").
subclass("aerodyne", "aircraft").

attribute("lift", "static", "aerostat").

subclass("fixed wing", "aerodyne").
subclass("rotorcraft", "aerodyne").

attribute("lift", "aerodynamic", "fixed wing").
attribute("lift", "rotor", "rotorcraft").

subclass("helicopter", "rotorcraft").
subclass("helicopter", "military").

subclass("glider", "fixed wing").
subclass("glider", "unpowered").
subclass("glider", "private").

subclass("aeroplane", "fixed wing").
subclass("aeroplane", "motorised").
subclass("aeroplane", "public").

subclass("propeller plane", "aeroplane").
subclass("jet plane", "aeroplane").

attribute("propulsion", "propeller", "propeller plane").

attribute("engine", "jet engine", "jet plane").
attribute("fuel", "avtur", "jet plane").
attribute("propulsion", "jet stream", "jet plane").

subclass("baloon", "aerostat").
subclass("baloon", "unpowered").
subclass("baloon", "private").


subclass("airship", "aerostat").
subclass("airship", "motorised").
subclass("airship", "public").

attribute("propulsion", "propeller", "airship").
object("LZ 129 Hindenburg", "airship").

%% water transport classes (empty)

%% subclass("submarine", "watercraft").
%% subclass("ship", "watercraft").

%% attribute("submersive", "true", "submarine").
%% attribute("submersive", "false", "ship").

%% land transport classes

subclass("train", "landcraft").
subclass("train", "public").

subclass("roadcraft", "landcraft").

attribute("route type", "road", "roadcraft").
attribute("route type", "railway", "railcraft").
attribute("propulsion", "wheel drive", "roadcraft").
attribute("propulsion", "wheel drive", "railcraft").

subclass("motor vehicle", "roadcraft").
subclass("motor vehicle", "motorised").

subclass("bicycle", "human-powered").
subclass("bicycle", "roadcraft").
subclass("bicycle", "private").

attribute("propulsion", "pedal power", "bicycle").
attribute("wheels", "2", "bicycle").

subclass("kick scooter", "human-powered").
subclass("kick scooter", "roadcraft").
subclass("lick scooter", "private").

attribute("propulsion", "foot power", "kick scooter").
attribute("wheels", "2", "kick scooter").

subclass("automobile", "motor vehicle").
subclass("automobile", "private").

subclass("motorcycle", "motor vehicle").
subclass("motorcycle", "petrol vehicle").
subclass("motorcycle", "private").

subclass("truck", "motor vehicle").
subclass("truck", "cargo").

subclass("diesel truck", "truck").
subclass("diesel truck", "diesel vehicle").

subclass("electric truck", "truck").
subclass("electric truck", "electric vehicle").

subclass("hybrid truck", "hybrid diesel electric").
subclass("hybrid truck", "truck").

attribute("wheels", "2", "motorcycle").
attribute("wheels", "4", "automobile").

subclass("electrocar", "motor vehicle").
subclass("electrocar", "electric vehicle").


%% logic
class_("transport").
class_(X):- subclass(X,Z), class_(Z).

isClass(X):- setof(Z, class_(Z), ZS), member(X,ZS).
isObject(O):- object(O, _).
isAttr(M):- setof(A, V^C^attribute(A,V,C), AS), member(M,AS).

addClass(Parent, Class):- assert(subclass(Class, Parent)).
rmvClass(Class):- retractall(subclass(Class,_)), retractall(subclass(_, Class)), rmvAttr(Class, _).

addObject(Parent, Object):- assert(object(Object, Parent)).
rmvObject(Object):- retractall(object(Object,_)), rmvAttr(Object, _).

getAttr(C, A, V):- attribute(A, V, C).
getAttr(C, A, V):- 
	(object(C, CC) ; subclass(C, CC)), 
	getAttr(CC, A, V), 
	not(attribute(A, _, C)).

allAttr(C, A_Vs):- setof(A-V, getAttr(C, A, V), A_Vs).
rmvAttr(C, A):-retractall(attribute(A,_,C)).
addAttr(A, V, C):- assert(attribute(A,V,C)).

takeUntil(Delim, [Delim|Cs],[], Cs).
takeUntil(_, [],[], []).
takeUntil(Delim, [C|Cs], [C|Out], Rest) :- takeUntil(Delim, Cs, Out, Rest).

intercalate([], _,  []).
intercalate([E], _,  [E]).
intercalate([E|ES], D , [E, D | Rest]):- intercalate(ES,D,Rest).


%% input - output


tokenize( [], [], []).
tokenize( ['\''|Cs], [], [T|Ts]) :- takeUntil('\'', Cs, Out, Rest), string_chars(T, Out), tokenize(Rest, [], Ts),!.
tokenize( ['"'|Cs], [], [T|Ts]) :- takeUntil('"', Cs, Out, Rest), string_chars(T, Out), tokenize(Rest, [], Ts),!.
tokenize( [' '|Cs], [], Ts):- tokenize(Cs, [], Ts),!.
tokenize( [' '|Cs], CurrInv, [T|Ts]):- reverse(CurrInv, Curr), string_chars(T, Curr),  tokenize(Cs, [], Ts),!.
tokenize( [], CurrInv, [T]):- reverse(CurrInv, Curr), string_chars(T, Curr).
tokenize( [C| Cs], CurrInv, Ts):- tokenize(Cs, [C|CurrInv], Ts),!.

printAttr([]):-nl.
printAttr([ (A-V) | Rest ]):-write(A),write(": "), write(V), nl, printAttr(Rest).

printList([]):-nl.
printList([E|ES]):-write(E), printList(ES).


myhelp:-nl,
	write("Commands:"),nl,
	write("	classes - lists existing classes"),nl,
	write("	objects - lists existing objects "),nl,
	write("	attributes - lists existing attributes"),nl,
	write("	info <class/object> [<attribute>]"),nl,
	write("	add class <class> <parent> - where parent is a class"),nl,
	write("	add object <object> <parent>"),nl,
	write("	add attribute <attribute> <value> <class/object>"),nl,
	write("	remove class <class>"),nl,
	write("	remove object <object>"),nl,
	write("	remove attribute <attribute> <class/object>"),nl,
	write("	compare <class/object> <class/object>"),nl,
	write("Examples:"), nl,
	write("	info \"jet plane\""),nl,
	write("	add attribute weight 9001 truck"),nl,nl.

:- initialization main.
main:-myhelp,loop.

loop:- 
	prompt(_, ''), 
	read_string(user_input, "\n", "\r", _, Input),
	string_chars(Input, Chars), tokenize(Chars, [], Tokens),
	process(Tokens),
	loop.

process([]).
process(["info" , CS]):- 
	allAttr(CS,A_Vs), 
	printAttr(A_Vs),!.
process(["info" , CS, AS]):-  
	setof(AS-V, getAttr(CS,AS,V), A_Vs), 
	printAttr(A_Vs),!.

process(["info" , CS, AS]):-(isObject(CS);isClass(CS)), write(CS), write(" does not have attribute "), write(AS),nl.

process(["objects"]):-
	setof(X, isObject(X), XS),
	intercalate(XS, ", ", Output),
	printList(Output),!.
process(["classes"]):-
	setof(X, isClass(X), XS),
	intercalate(XS, ", ", Output),
	printList(Output),!.
process(["attributes"]):-
	setof(X, isAttr(X), XS),
	intercalate(XS, ", ", Output),
	printList(Output),!.

process(["add", "class", Name, Parent]):-addClass(Parent, Name).
process(["add", "object", Name, Class]):-addObject(Class, Name).
process(["add", "attribute", Attr, Val, Target]):-addAttr(Attr, Val, Target).

process(["remove", "class", Name]):-rmvClass(Name).
process(["remove", "object", Name]):-rmvObject(Name).
process(["remove", "attribute", Attr, Target]):-rmvAttr(Target, Attr).

process(["compare" , X1, X2]):-
	allAttr(X1,A_V1s), 
	allAttr(X2,A_V2s), 
	cmpAttr(A_V1s,A_V2s,[], Sim),
	write(X1),write(":"),nl,nl, printAttrExcept(A_V1s,Sim), nl,
	write(X2),write(":"),nl,nl, printAttrExcept(A_V2s,Sim), nl,
	!.

process(["exit"]):-halt.
process(["help"]):-myhelp.

process(_):- write("unknow command"),nl.


printAttrExcept([],_):-nl.
printAttrExcept([A-V|XS], Ex):- not(member(A,Ex)), write(A),write(": "), write(V), nl, printAttrExcept(XS,Ex),!.
printAttrExcept([_|XS], Ex):- printAttrExcept(XS,Ex),!.

cmpAttr([], _, _, []).
cmpAttr([A-V|XS], YS, Dif, Res):- member(A-V,YS), not(member(A, Dif)), cmpAttr(XS,YS, Dif, Res2), union([A],Res2, Res),!.
cmpAttr([A-_|XS], YS, Dif, Res):- union([A],Dif, Dif2), cmpAttr(XS,YS, Dif2, Res2), subtract(Res2, [A], Res),!.