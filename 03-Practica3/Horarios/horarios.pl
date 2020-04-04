% INPUT
% Sintaxi: assig(curs,assignatura,hores,llistaAules,llistaProfessors).
assig(1,1,2,[1,2,3],[1,3]).
assig(1,2,2,[2,3],[5]).
assig(1,3,4,[2,3],[1,3,4,5]).
assig(1,4,3,[1],[1,2,4,5]).
assig(1,5,3,[1,2,3],[1,2,3,4,5]).

assig(2,6,3,[3],[1,3,5]).
assig(2,7,3,[1,2,3],[3,4,5]).
assig(2,8,4,[2,3],[3,4]).
assig(2,9,3,[2],[1,2,3,5]).
assig(2,10,3,[1,2,3],[4,5]).
assig(2,11,2,[1,3],[1,3,5]).

assig(3,12,3,[1,3],[1,3,4,5]).
assig(3,13,4,[1,2],[1,3,5]).
assig(3,14,2,[1,2,3],[1,2,4,5]).
assig(3,15,4,[3],[1,2,3]).
assig(3,16,3,[1,2,3],[2,5]).
assig(3,17,3,[1,2,3],[1]).
assig(3,18,3,[1,2,3],[3]).

assig(4,19,4,[1,2,3],[3]).
assig(4,20,4,[3],[1,3,4,5]).
assig(4,21,3,[2,3],[2,3]).
assig(4,22,2,[2,3],[2,3,4,5]).
assig(4,23,4,[1],[3]).

% Sintaxi: horesProhibides(professor,llistaHores).
horesProhibides(1,[4,7,12,15,16,18,26,29,30,37,38,45,50,54]).
horesProhibides(2,[1,5,6,9,11,13,17,20,25,29,30,32,33,37,38,42,44,49,50,55,57]).
horesProhibides(3,[5,7,8,10,21,22,25,34,38,39,60]).
horesProhibides(4,[4,9,10,14,17,20,21,22,24,25,27,31,33,38,39,41,42,43,47,55,57]).
horesProhibides(5,[2,20,26,27,30,31,44,53,56,58]).

numCursos(4).
numAssignatures(23).
numAules(3).
numProfes(5).
% ################################################################################################
symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

/*
    RESTRICCIONES TEXTUALES
*/

%%%%%% definitions:
course(C)   :- numAssignatures(X),  between(1,X,C).     % assignatura
room(R)     :- numAules(X),         between(1,X,R).     % aula
lecture(C,L):- assig(_,C,N,_,_),    between(1,N,L).     % sesion (1,2,3,..)
teacher(T)  :- numProfes(X),        between(1,X,T).     % profesor
slot(S)     :- between(1,60,S).                         % hora
year(Y)     :- numCursos(N),        between(1,N,Y).
dia(D)      :- between(1,5,D).                          % dia



%%%%%%  1. SAT Variables:
satVariable( cls(C,L,S) ):- course(C), lecture(C,L), slot(S).   % la classe número L de l'assignatura C s'imparte a l'slot S
satVariable( cr(C,R) ):- course(C), room(R).                    % assignatura C és impartida en l'aula R
satVariable( ct(C,T) ):- course(C), teacher(T).                 % assignatura C és impartida pel professor T
satVariable( cs(C,S) ):- course(C), slot(S).                    % asignatura C se imparte en el slot S
% satVariable( rs(R,S) ):- room(R), slot(S).                      % aula R esta ocupada en el slot S
%%%%%%  2. Clause generation:

writeClauses:- 
    programarLectures,
    nLectures,
    consistenciaCS,
    unaHoraPorDia,          % Maximo una hora de clase por asignatura al dia.
    unaClaseUnProfe,        % Una asignatura debe impartirse en la misma aula y por el mismo profesor.
    unaSalaUnaClase,        % En una aula y hora concretas, solo se puede impartir una clase.
    noSolapamientos,        % Dado un curso (year) y hora (slot) concretos, solo se puede impartir una asignatura (course).
    % Dado un profesor y hora concretos, solo se puede impartir una asignatura.
    % Dadas las asignaturas de un curso, no pueden quedar horas muertas.
    % Un curso no puede tener mas de 6 horas de clase al dia.
    % Control del numero de asignaturas
    true,!.
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

myDisplay(1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuantas
slot_del_dia(S,D)   :- F is div(S-1,12) + 1, D == F.



programarLectures:-
    lecture(C,L),
    findall(cls(C,L,S), slot(S), Lits),
    exactly(1,Lits),
    fail.
programarLectures.

nLectures:-
    slot(S),
    lecture(C,L1),
    lecture(C,L2),
    dif(L1,L2),
    findall(cls(C,L1,S), true, Lits1),
    findall(cls(C,L2,S), true, Lits2),
    append(Lits1, Lits2, Lits),
    exactly(1,Lits),
    fail.
nLectures.

consistenciaCS:-
    lecture(C,L), slot(S),
    expressAnd(cs(C,S), [cls(C,L,S)]),
    fail.
consistenciaCS.

unaHoraPorDia:-
    dia(D),
    course(C),
    findall( cls(C,S), (slot(S), slot_del_dia(S,H), H = D), Lits),
    atMost(1,Lits),
    fail.
unaHoraPorDia.

unaClaseUnProfe:-
    assig(_,C,_,A,P),
    course(C),
    findall( cr(C,R), (room(R), member(R,A)), Lits1),
    exactly(1,Lits1),
    findall( ct(C,T), (teacher(T), member(T,P)), Lits2),
    exactly(1,Lits2),
    fail.
unaClaseUnProfe.

unaSalaUnaClase:-
    slot(S), room(R),
    findall(rs(R,S), false ,Lits),
    atMost(1,Lits),
    fail.
unaSalaUnaClase.

noSolapamientos:-
    year(Y),
    slot(S),
    findall(cs(C,S), assig(Y,C,_,_,_), Lits),
    atMost(1,Lits),
    fail.
noSolapamientos.





%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:
extraBlank(N):- 
    N < 10, !, write(' ').
extraBlank(_).

drawTail(Y, Hour):-
    Hour > 48, 
    write('  Curs: '), write(Y), nl.
drawTail(_, _).

drawCell(Y, S, M):-
    member(cls(C,L,S), M),                   %% -------- ADAPTA la SAT variable cls(C,L,S)
    assig(Y, C, _, _, _), !,
    write(' '), extraBlank(C), write(C), write(' - '),
    extraBlank(L), write(L), 
    write('  ['), member(cr(C,R), M),        %% -------  ADAPTA la SAT variable cr(C,R)
    write('A:'), extraBlank(R), write(' '), write(R), write(']'),
    write('  ['), member(ct(C,T), M),        %% -------  ADAPTA la SAT variable ct(C,T)
    write('P:'), extraBlank(T), write(' '), write(T), write(']'),
    write(' ').
drawCell(_, _, _):- 
    write('                           ').    

drawRow(Row, _):-
    1 is Row mod 2,
    H is Row // 2 + 8, 
    extraBlank(H), 
    write(' '), write(H), write(':00 '), 
    between(1, 141, _), write('='), 
    fail.
drawRow(Row, _):-
    1 is Row mod 2, !, nl.

drawRow(Row, M):-
    year(Y),
    write('       |'),
    between(0, 4, Day), 
    Hour is Row // 2 + Day * 12,
    drawCell(Y, Hour, M), 
    write('|'), 
    drawTail(Y, Hour), 
    fail.
drawRow(_, _).

drawHeader:-
    nl, nl, 
    write(' Format de sortida: Assignatura - Hora [A: Aula] [P: Professor]'), 
    nl, nl, 
    write('                 Dilluns                     Dimarts                     dimecres                     Dijous                    Divendres').

displaySchedule(M):-
    drawHeader, nl,
    between(1, 25, Row), 
    drawRow(Row, M), 
    fail.

drawHeaderYear(Y):-
    nl, nl, 
    write('----------------------------------------------------------------------------------------------------------------------------------------------------'),
    nl,
    write(' Horari del curs '), write(Y),
    nl,
    write(' Format de sortida: Assignatura - Hora [A: Aula] [P: Professor]'), 
    nl, nl, 
    write('                 Dilluns                     Dimarts                     dimecres                     Dijous                    Divendres').

drawTailYear(Hour):-
    Hour > 48, nl.
drawTailYear(_, _).

drawRowYear(Row, _, _):-
    1 is Row mod 2,
    H is Row // 2 + 8, 
    extraBlank(H), 
    write(' '), write(H), write(':00 '), 
    between(1, 141, _), write('='), 
    fail.
drawRowYear(Row, _, _):-
    1 is Row mod 2, !, nl.
drawRowYear(Row, Y, M):-
    write('       |'),
    between(0, 4, Day), 
    Hour is Row // 2 + Day * 12,
    drawCell(Y, Hour, M), 
    write('|'),
    drawTailYear(Hour), 
    fail.
drawRowYear(_, _, _).

displayScheduleYear(Y,M):-
    drawHeaderYear(Y), nl,
    between(1, 25, Row), 
    drawRowYear(Row, Y, M), 
    fail.

displaySol(M):-
    myDisplay(1),
    course(C), lecture(C,L), slot(S), room(R), teacher(T),
    member(cls(C,L,S), M), member(cr(C,R), M), member(ct(C,T), M), member(cs(C,S), M),
    write('Assig: '), write(C),
    write('.'), write(L),
    write('\tSlot: '), write(S),
    write('\tTeacher: '), write(T),
    write('\tRoom: '), write(R),
    nl,
    fail.
displaySol(_):- myDisplay(1).


displaySol(M):- displaySchedule(M), fail.
displaySol(M):- year(Y), displayScheduleYear(Y,M), fail.
displaySol(_).



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
