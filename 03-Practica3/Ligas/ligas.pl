symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.
% ################################################################################################
numEquipos(16).
nofuera(7,10).
nofuera(6,10).
nofuera(9,10).
nofuera(10,10).
nofuera(11,10).
nofuera(7,30).
nofuera(6,30).
nofuera(9,30).
nofuera(10,30).
nofuera(11,30).
nocasa(7,1).
nocasa(8,1).
nocasa(9,1).
nocasa(10,1).
nocasa(11,1).
nocasa(12,1).
norepes(1,2).
norepes(2,3).
norepes(28,29).
norepes(29,30).
nopartido(1,2,30).
nopartido(1,2,1).
nopartido(1,2,2).
nopartido(1,2,3).
nopartido(1,2,4).
nopartido(1,2,5).
nopartido(1,2,6).
nopartido(1,2,7).
sipartido(2,3,30).
sipartido(4,5,30).
% ################################################################################################

%%%%%% Some helpful definitions to make the code cleaner:
numpartidos(N) :- numEquipos(X), N is 2*X - 2.
equipo(I)   :- numEquipos(X), M is X, between(1,M,I).
jornada(J)  :- numpartidos(X), N is X, between(1,N,J).

%%%%%%  1. SAT Variables:

% partido(I1,I2,J,V) means "equipo I1 juega en casa contra equipo I2 (que juega fuera) en la jornada J de la vuelta V"
satVariable( partido(I1,I2,J) )  :- equipo(I1), equipo(I2), jornada(J).

%%%%%%  2. Clause generation:

writeClauses:- 
    unPartidoPorIda,           % Dos equipos diferentes se enfrentan 1 sola jornada en toda la ida
    unPartidoPorJornada,       % Un equipo en una jornada, solo participa en un partido
    peticionNoCasa,            % Un equipo E juega la jornada J en casa si y solo si lo pide.
    peticionNoFuera,           % Un equipo E juega la jornada J fuera si y solo si lo pide.
    peticionNoConcreto,        
    peticionSiConcreto,
    sinRepetir,
    noTripeticiones,
    espejo,                    % Dos equipos se enfrentan en una jornada de ida si y solo si se enfrentan en la jornada analoga (vuelta) al reves
    true,!.                    % this way you can comment out ANY previous line of writeClauses
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Precicados ciertos si la jornada pertenece al periodo de ida o vuelta respectivamente
ida(J):- numpartidos(N), H is N/2, between(1,H,J).
vuelta(J):- numpartidos(N), H is 1 + N/2, between(H,N,J).

analogo(J,V):- ida(J), numpartidos(N), V is J + N/2, !.
analogo(J,V):- vuelta(J), numpartidos(N), V is J - N/2, !.

unPartidoPorIda:-
    equipo(I1),
    equipo(I2),
    dif(I1,I2),

    findall(partido(I1,I2,J), (jornada(J), ida(J)), Lits1),
    findall(partido(I2,I1,J), (jornada(J), ida(J)), Lits2),
    append(Lits1, Lits2, Lits),
    exactly(1,Lits),
    
    fail.
unPartidoPorIda.

unPartidoPorJornada:-
    equipo(I1),
    jornada(J),
    findall(partido(I1,I2,J), equipo(I2), Lits1),
    findall(partido(I2,I1,J), equipo(I2), Lits2),
    append(Lits1, Lits2, Lits),
    exactly(1,Lits),
    fail.
unPartidoPorJornada.

peticionNoCasa:-
     equipo(I1),
     jornada(J),
     nocasa(I1,J),
     findall(partido(I1,I2,J), equipo(I2), Lits),
     atMost(0,Lits),
     fail.
peticionNoCasa.

peticionNoFuera:-
    equipo(I1),
    jornada(J),
    nofuera(I1,J),
    findall(partido(I2,I1,J), equipo(I2), Lits),
    atMost(0,Lits),
    fail.
peticionNoFuera.

peticionNoConcreto:-
    equipo(I1),
    equipo(I2),
    jornada(J),
    nopartido(I1,I2,J),
    findall(partido(I1,I2,J), true, Lits),
    atMost(0,Lits),
    fail.
peticionNoConcreto.

peticionSiConcreto:-
    equipo(I1),
    equipo(I2),
    jornada(J),
    sipartido(I1,I2,J),
    findall(partido(I1,I2,J), true, Lits),
    exactly(1,Lits),
    fail.
peticionSiConcreto.

sinRepetir:-
    equipo(I1),
    norepes(J1,J2),
    findall(partido(I1,I2,J1), (equipo(I2)), Lits1),
    findall(partido(I1,I3,J2), (equipo(I3)), Lits2),
    append(Lits1, Lits2, Lits),
    exactly(1,Lits),
    fail.
sinRepetir.

noTripeticiones:-
    equipo(I1),
    numpartidos(N),
    F is N - 2,
    between(1,F,J1), % Compruebo que la jornada no sea de las dos ultimas
    J2 is J1 + 1,
    J3 is J2 + 1,
    findall(partido(I1,I2,J1), (equipo(I2)), Lits1),
    findall(partido(I1,I3,J2), (equipo(I3)), Lits2),
    findall(partido(I1,I4,J3), (equipo(I4)), Lits3),
    append(Lits1, Lits2, Lits4), append(Lits3, Lits4, Lits),
    atLeast(1,Lits),
    atMost(2,Lits),
    fail.
noTripeticiones.

% Calculamos todo sobre la ida, y duplicamos la vuelta
espejo:- 
    equipo(I1),
    equipo(I2),
    jornada(J),
    analogo(J,V),
    expressAnd(partido(I1,I2,J), [partido(I2,I1,V)]),
    fail.
espejo.



%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

displaySol(M):- 
    equipo(I1), equipo(I2), jornada(J),
    member(partido(I1,I2,J), M ),
    nl, write('Jornada '), digitos(J), write(' Partido: '), digitos(I1), write(' vs. '), digitos(I2), nl,
    fail.
displaySol(_):- nl,nl.

digitos(N):- member(N,[1,2,3,4,5,6,7,8,9]), write('0'), write(N),!.
digitos(N):- write(N).


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
