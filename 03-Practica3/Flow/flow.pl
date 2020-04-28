%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% INPUT 1
size(9).
c(blue,  9,1,2,2).
c(brown, 3,1,8,4).
c(red,   3,4,1,7).
c(cyan,  1,8,4,4).
c(green, 1,9,5,2).
c(yellow,7,7,7,9).
c(pink,  6,5,8,7).
c(violet,8,9,9,6).
c(orange,5,8,8,8).

%%% INPUT 2
% size(14).
% c( blue,       9,10, 12,11).
% c( brown,      4,9,  14,10).
% c( red,        6,4,  7,8  ).
% c( cyan,       7,3,  7,5  ).
% c( green,      6,7,  8,8  ).
% c( yellow,     8,1,  5,11 ).
% c( pink,       11,3, 12,5 ).
% c( violet,     5,2,  13,13).
% c( orange,     11,4, 9,8  ).
% c( darkblue,   2,2,  2,6  ).
% c( darkgreen,  10,5, 10,8 ).
% c( darkred,    14,11,11,14).
% c( darkcyan,   6,5,  3,12 ).
% c( white,      9,5,  8,12 ).
% c( grey,       14,8, 10,10).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

















%%%%%% DEFINITIONS
coord(C)    :- size(N), between(1,N,C).
cell(X,Y)   :- coord(X), coord(Y).
slot(S)		:- size(N), M is N*N, between(1,M,S).
cs(X,Y,S)	:- size(N), S is (X-1)*N + Y.
color_(C)   :- c(C,_,_,_,_).

%%%%%%  1. SAT Variables:
satVariable( color(X,Y,C) )			:- coord(X), coord(Y), color_(C).
satVariable( start_color(X,Y,C) )	:- coord(X), coord(Y), color_(C).
satVariable( end_color(X,Y,C) )		:- coord(X), coord(Y), color_(C).
satVariable( pair(S1,S2))			:- slot(S1), slot(S2).
satVariable( pc(S1,S2,C))			:- slot(S1), slot(S2), color_(C).
satVariable( connected(S1,S2))      :- slot(S1), slot(S2).
satVariable( continuous(S1,S2,S3))  :- slot(S1), slot(S2), slot(S3).

%%%%%%  2. Clause generation:
writeClauses:- 
    start_end_colors,	% Los colores iniciales, son del color que marcan.
	full,				% Toda celda es de un color
	emparejados,		% Una celda es pareja izquierda de una sola celda, y es pareja derecha de una sola celda.
	pairColor,			% Si una celda pertecene a una pareja, y es del color C, entonces la pareja es del color C.
	adjacent_connection,
	connected,
	% linea_horizontal,
	% linea_vertical,
	% vecinos,
	% esquinas,
    true,!.
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

implica(A,B):- writeClause([-A,B]).

adjacent(X,Y1,X,Y2):- cell(X,Y1), cell(X,Y2), Y2 is Y1 + 1, !.
adjacent(X,Y1,X,Y2):- cell(X,Y1), cell(X,Y2), Y2 is Y1 - 1, !.
adjacent(X1,Y,X2,Y):- cell(X1,Y), cell(X2,Y), X2 is X1 + 1, !.
adjacent(X1,Y,X2,Y):- cell(X1,Y), cell(X2,Y), X2 is X1 - 1, !.

start_end_colors:-

	c(C,XS,YS,XE,YE), cs(XS,YS,Start), cs(XE,YE,End),
	
	% Escribir inicio y final
	writeClause([color(XS,YS,C)]),
	writeClause([color(XE,YE,C)]),

	% Pair
	writeClause([pair(End,Start)]), 
	writeClause([pc(End,Start,C)]),
	writeClause([connected(Start,End)]),
	writeClause([connected(End,Start)]),
	% findall(pair(Start,Aux), (cell(X,Y), adjacent(X,Y,XS,YS), cs(X,Y,Aux)), Lits1), exactly(1,Lits1),
	% findall(pc(Start,Aux,C), (cell(X,Y), adjacent(X,Y,XS,YS), cs(X,Y,Aux)), Lits2), exactly(1,Lits2),

	% findall(pair(Aux,End), (cell(X,Y), adjacent(X,Y,XE,YE), cs(X,Y,Aux)), Lits3), exactly(1,Lits3),
	% findall(pc(Aux,End,C), (cell(X,Y), adjacent(X,Y,XE,YE), cs(X,Y,Aux)), Lits4), exactly(1,Lits4),

    fail.
start_end_colors.

full:-
	cell(X,Y),
	findall(color(X,Y,C), color_(C), Lits),
	exactly(1,Lits),
	fail.
full.

emparejados:-
	c(_,_,_,X1,Y1), cs(X1,Y1,End),% La ficha End
	findall(pair(S2,End), (cell(X2,Y2), adjacent(X1,Y1,X2,Y2), cs(X2,Y2,S2)), Lits1),
	exactly(1,Lits1),
	fail.
emparejados:-
	cell(X1,Y1), not(c(_,_,_,X1,Y1)), cs(X1,Y1,S1),  % Toda celda a excepcion del end
	findall(pair(S1,S2), (cell(X2,Y2), adjacent(X1,Y1,X2,Y2), cs(X2,Y2,S2), not(c(_,X2,Y2,_,_))), Lits1),
	exactly(1,Lits1),
	fail.
emparejados.

pairColor:-
	color_(C), cell(X1,Y1), cell(X2,Y2), adjacent(X1,Y1,X2,Y2), cs(X1,Y1,S1), cs(X2,Y2,S2),
	% expressAnd(pair(S1,S2), [pair(S2,S1)]),
	% expressAnd(pc(S1,S2,C), [pc(S2,S1,C)]),
	expressAnd(pc(S1,S2,C), [pair(S1,S2), color(X1,Y1,C)]),
	writeClause([-pc(S1,S2,C),color(X2,Y2,C)]),
	writeClause([-pc(S1,S2,C),color(X1,Y1,C)]),
	fail.
pairColor.

adjacent_connection:-
	cell(X1,Y1), cell(X2,Y2), cell(X3,Y3), adjacent(X1,Y1,X2,Y2), adjacent(X2,Y2,X3,Y3), cs(X1,Y1,S1), cs(X2,Y2,S2), cs(X3,Y3,S3),
	expressAnd( continuous(S1,S2,S3), [pair(S1,S2), pair(S2,S3)]),
	expressAnd( continuous(S1,S2,S3), [connected(S1,S2), connected(S2,S3)]),
	% writeClause([-continuous(S1,S2,S3), connected(S1,S3)]),
	fail.
adjacent_connection.

connected:-
	cell(X1,Y1), cell(X2,Y2), cell(X3,Y3), cs(X1,Y1,S1), cs(X2,Y2,S2), cs(X3,Y3,S3),
	expressAnd( continuous(S1,S2,S3), [connected(S1,S2), connected(S2,S3)]),
	writeClause([-continuous(S1,S2,S3), connected(S1,S3)]),
	fail.
connected.




























%%% 3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

displaySol(_):- nl,nl, write('Input:   '), coord(X), nl, coord(Y), writeInputSq(X,Y), fail. 

displaySol(M):- nl,nl, write('Datos:'), slot(S1), slot(S2), cell(X1,Y1), cell(X2,Y2), adjacent(X1,Y1,X2,Y2), cs(X1,Y1,S1), cs(X2,Y2,S2),
				member(connected(S1,S2),M), member(color(X1,Y1,C1),M), member(color(X2,Y2,C2),M), nl,
				setColor(white), write('pair('), setColor(C1), write(X1), write(','), write(Y1), write(','), setColor(C2), write(X2), write(','), write(Y2), write(')'), fail. 

displaySol(M):- nl,nl, write('Solution:'), coord(X), nl, coord(Y),
		member(color(X,Y,Color),M), setColor(Color), write(' o'), fail. 
displaySol(_):- resetColor, !.

writeInputSq(X,Y):- c(Color,X,Y,_,_), setColor(Color), write(' o'), !.
writeInputSq(X,Y):- c(Color,_,_,X,Y), setColor(Color), write(' o'), !.
writeInputSq(_,_):- resetColor, write(' ·'), !.

setColor(Color):- colorCode(Color,Code), put(27), write('[0;38;5;'), write(Code), write('m'), !.
resetColor:- put(27), write('[0m'), !.

colorCode( blue,       69  ).
colorCode( brown,      138 ).
colorCode( red,        196 ).
colorCode( cyan,       51  ).
colorCode( green,      46  ).
colorCode( yellow,     226 ).
colorCode( pink,       201 ).
colorCode( violet,     90  ).
colorCode( orange,     208 ).
colorCode( darkblue,   21  ).
colorCode( darkgreen,  28  ).
colorCode( darkred,    88  ).
colorCode( darkcyan,   30  ).
colorCode( white,      15  ).
colorCode( grey,       8   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving 
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !. 
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> and('), write(Lits), write(')'), nl, !. 
expressAnd( Var, Lits):- member(Lit,Lits), negate(Var,NVar), writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits):- findall(NLit, (member(Lit,Lits), negate(Lit,NLit)), NLits), writeClause([ Var | NLits]), !.


%%%%%% Cardinality constraints on arbitrary sets of literals Lits:

exactly(K,Lits):- symbolicOutput(1), write( exactly(K,Lits) ), nl, !.
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):- symbolicOutput(1), write( atMost(K,Lits) ), nl, !.
atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
	negateAll(Lits,NLits),
	K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):- symbolicOutput(1), write( atLeast(K,Lits) ), nl, !.
atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
	length(Lits,N),
	K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate( -Var,  Var):-!.
negate(  Var, -Var):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).


%%%%%% main:

main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt
main:-  initClauseGeneration,
tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
tell(header),  writeHeader,  told,
numVars(N), numClauses(C),
write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
shell('cat header clauses > infile.cnf',_),
write('Calling solver....'), nl,
shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
	treatResult(Result),!.

treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.
treatResult( _):- write('cnf input error. Wrote anything strange in your cnf?'), nl,nl, halt.
    

initClauseGeneration:-  %initialize all info about variables and clauses:
	retractall(numClauses(   _)),
	retractall(numVars(      _)),
	retractall(varNumber(_,_,_)),
	assert(numClauses( 0 )),
	assert(numVars(    0 )),     !.

writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w(-Var):- symbolicOutput(1), satVariable(Var), write(-Var), write(' '),!. 
w( Var):- symbolicOutput(1), satVariable(Var), write( Var), write(' '),!. 
w(-Var):- satVariable(Var),  var2num(Var,N),   write(-), write(N), write(' '),!.
w( Var):- satVariable(Var),  var2num(Var,N),             write(N), write(' '),!.
w( Lit):- told, write('ERROR: generating clause with undeclared variable in literal '), write(Lit), nl,nl, halt.


% given the symbolic variable V, find its variable number N in the SAT solver:
:-dynamic(varNumber / 3).
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.

% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
