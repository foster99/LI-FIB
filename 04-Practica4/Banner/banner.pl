:-dynamic(varNumber / 3).
symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

%% A factory produces banners using only a set of existing rectangular
%% pieces.  Our goal is to find out how to use the minimum set of
%% pieces that exactly match the design of the banner. Note that
%% pieces can be rotated if necessary. Also, note that each piece can
%% be used at most once.  That's why there can be several identical
%% pieces in the input.

%%%%%%%%%%%%%%%%%%%%% INPUT EXAMPLE:

banner( [                             % the x's define the design of the banner
	       [.,x,x,x,.,.,.,x,x,x,.],
	       [.,x,x,x,.,.,.,x,x,x,.],
	       [.,x,x,x,.,.,.,x,x,x,.],
	       [.,x,x,x,x,x,x,x,x,x,.],
	       [.,x,x,x,x,x,x,x,x,x,.],
	       [.,x,x,x,.,.,.,x,x,x,.],
	       [.,x,x,x,.,.,.,x,x,x,.],
	       [.,x,x,x,.,.,.,x,x,x,.]
	   ]).
pieces([
	    [1,3,8],   % piece 1 is a 3 x 8 rectangle
	    [2,3,3],   % piece 2 is a 3 x 3 rectangle
	    [3,9,2],   % ...
	    [4,3,3],
	    [5,3,8],
	    [6,3,2],
	    [7,3,3],
	    [8,3,3],
	    [9,2,3],
	    [a,1,3]	    
	  ]).

%% A possible solution using 6 pieces:
%% .444...888.
%% .444...888.
%% .444...888.
%% .333333333.
%% .333333333.
%% .a99...777.
%% .a99...777.
%% .a99...777.

%% An optimal solution using 3 pieces:
%% .555...111.
%% .555...111.
%% .555...111.
%% .555666111.
%% .555666111.
%% .555...111.
%% .555...111.
%% .555...111.
      	 

%%%%%% Some helpful definitions to make the code cleaner:
piece(P):-                  pieces(L), member([P,_,_],L).
pieceSize(P,W,H):-          pieces(L), member([P,W,H],L).
widthBanner(W):-            banner(B), member(L,B), length(L,W),!.
heightBanner(H):-           banner(B), length(B,H), !.
contentsCellBanner(X,Y,C):- cell(X,Y), banner(B), heightBanner(H), Y1 is H-Y+1, nth1(Y1,B,L), nth1(X,L,C).
cell(X,Y):-                 widthBanner(W), heightBanner(H), between(1,W,X), between(1,H,Y).
invalidX(X,W):-             widthBanner(WB), X+W-1 > WB.
invalidY(Y,H):-             heightBanner(HB), Y+H-1 > HB.
%% Hay que usar mas definiciones para facilitar la redaccion de las restricciones.

satVariable( pieceStarts(P,X,Y) )   :- piece(P), cell(X,Y).                 % la pieza P tiene su esquina inferior izquierda en la posicion (X,Y)
satVariable( rotated(P) )           :- piece(P).                            % la pieza P esta rotada
satVariable( pieceCell(P,X,Y) )     :- piece(P), cell(X,Y).                 % la pieza P ocupa la celda (X,Y)
satVariable( used(P) )              :- piece(P).                            % la pieza P esta en uso



%%%%%%%%%%%%%%%%%

writeClauses(infinite):- !, writeClauses(10),!.
writeClauses(MaxPieces):-
    noExceedLimits,         %% cap pesa esta situada en una posicio que fa que surti fora del banner (fa que su X,Y, invalids), hay que contemplar si esta rotada o no
    onlyOnePos,             %% una pieza solo puede estar en una posicion
    onlyBannerCells,        %% en toda celda sin banner no hay pieza y en toda pieza con banner solo puede haber parte de 1 pieza
    pieceSpace,             %% Si una pieza P esta en la posicion (X,Y), entonses ocupa espasio xd
    % rotatedAndLocated,      %% Escribe la relacion entre estar colocada y rotada
    onlyIfUsed,             %% Si una pieza esta colocada en algun sitio, esta esta en uso
    atMostUsed(MaxPieces),  %% No se usan mas de K piezas
    true, !.
writeClauses(_):- told, nl, write('writeClauses failed!'), nl,nl, halt.
%%%%%%%%%%%%%%%%%

noExceedLimits:- piece(P), cell(X,Y), pieceSize(P,W,_), invalidX(X,W), writeClause([ rotated(P), -pieceStarts(P,X,Y)]), fail.
noExceedLimits:- piece(P), cell(X,Y), pieceSize(P,_,H), invalidX(X,H), writeClause([-rotated(P), -pieceStarts(P,X,Y)]), fail.
noExceedLimits:- piece(P), cell(X,Y), pieceSize(P,_,H), invalidY(Y,H), writeClause([ rotated(P), -pieceStarts(P,X,Y)]), fail.
noExceedLimits:- piece(P), cell(X,Y), pieceSize(P,W,_), invalidY(Y,W), writeClause([-rotated(P), -pieceStarts(P,X,Y)]), fail.
noExceedLimits.

onlyBannerCells:- cell(X,Y), contentsCellBanner(X,Y,x), findall(pieceCell(P,X,Y), piece(P), Lits), exactly(1,Lits), fail.
onlyBannerCells:- cell(X,Y), contentsCellBanner(X,Y,.), findall(pieceCell(P,X,Y), piece(P), Lits),  atMost(0,Lits), fail.
onlyBannerCells.

pieceSpace:- % Pieza no rotada, celda incluida
    piece(P), cell(Xstart,Ystart), pieceSize(P,W,H), Xend is Xstart + W - 1, Yend is Ystart + H - 1,
    cell(X,Y), between(Xstart,Xend,X), between(Ystart,Yend,Y),
    writeClause([rotated(P), -pieceStarts(P,Xstart,Ystart), pieceCell(P,X,Y)]),
    fail.
pieceSpace:- % Pieza no rotada, celda no incluida
    piece(P), cell(Xstart,Ystart), pieceSize(P,W,H), Xend is Xstart + W - 1, Yend is Ystart + H - 1,
    cell(X,Y), not(between(Xstart,Xend,X)), not(between(Ystart,Yend,Y)),
    writeClause([rotated(P), -pieceStarts(P,Xstart,Ystart), -pieceCell(P,X,Y)]),
    fail.
pieceSpace:- % Pieza rotada, celda incluida
    piece(P), cell(Xstart,Ystart), pieceSize(P,H,W), Xend is Xstart + W - 1, Yend is Ystart + H - 1,
    cell(X,Y), between(Xstart,Xend,X), between(Ystart,Yend,Y),
    writeClause([-rotated(P), -pieceStarts(P,Xstart,Ystart), pieceCell(P,X,Y)]),
    fail.
pieceSpace:- % Pieza rotada, celda no incluida
    piece(P), cell(Xstart,Ystart), pieceSize(P,H,W), Xend is Xstart + W - 1, Yend is Ystart + H - 1,
    cell(X,Y), not(between(Xstart,Xend,X)), not(between(Ystart,Yend,Y)),
    writeClause([-rotated(P), -pieceStarts(P,Xstart,Ystart), -pieceCell(P,X,Y)]),
    fail.
pieceSpace.

onlyOnePos:- 
    piece(P), % pieceSize(P,W,H), Area is W*H,
    findall(pieceStarts(P,X,Y), cell(X,Y), Lits1), atMost(1,Lits1),
    % findall(pieceCell(P,X,Y), cell(X,Y), Lits2), atMost(Area,Lits2),
    fail.
onlyOnePos.

onlyIfUsed:-
    piece(P),
    findall(pieceCell(P,X,Y), cell(X,Y), Lits1), expressOr(used(P), Lits1),
    findall(pieceStarts(P,X,Y), cell(X,Y), Lits2), expressOr(used(P), Lits2),
    fail.
onlyIfUsed.

useThePiece:- piece(P), cell(X,Y), writeClause([-pieceStarts(P,X,Y), used(P)]), fail.
useThePiece.

atMostUsed(MaxPieces):- findall(used(P), piece(P), Lits), atMost(MaxPieces, Lits).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4. This predicate computes the cost of a given solution M:

costOfThisSolution(M,Cost):- findall(P, member(used(P),M),L), length(L,Cost), !.

%%%%%%%%%%%%%%%%%
% displaySol(M):-
%     cell(X,Y), piece(P),between(0,1,R),
%     member(pieceStartR(P,X,Y,R),M), write('StartR: ('), write(X), write(','), write(Y), write(') -> R('), write(R), write(') '), write(P), nl,
%     fail.
displaySol(M):-
    cell(X,Y), piece(P),
    member(pieceStarts(P,X,Y),M), member(rotated(P),M), write('Start: ('), write(X), write(','), write(Y), write(') '), write(P), write(' R'), nl,
    fail.
displaySol(M):-
    cell(X,Y), piece(P),
    member(pieceStarts(P,X,Y),M), not(member(rotated(P),M)), write('Start: ('), write(X), write(','), write(Y), write(') '), write(P), nl,
    fail.
displaySol(M):-
    cell(X,Y), piece(P),
    member(pieceCell(P,X,Y), M), write('pc: ('), write(X), write(','), write(Y), write(') -> '), write(P), nl,
    fail.
displaySol(M):-
    widthBanner(W),
    heightBanner(H),
    between(1,H,YB),
    nl,
    Y is H-YB+1,
    between(1,W,X),
    writeCell(M,X,Y),
    fail.
displaySol(_):-nl.

writeCell(M,X,Y):- member(pieceCell(P,X,Y),M), !, write(P).
writeCell(_,_,_):- write('.').

%% JESUCRISTO HA NACIDO: writeClause([-a,b]) === a => b




































%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% No need to modify anything beow this line:

main:-  symbolicOutput(1), !, writeClauses(infinite), halt.   % print the clauses in symbolic form and halt
main:-
    told, write('Looking for initial solution with arbitrary cost...'), nl,
    initClauseGeneration,
    tell(clauses), writeClauses(infinite), told,
    tell(header),  writeHeader,  told,
    numVars(N), numClauses(C), 
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result,[]),!.

treatResult(20,[]       ):- write('No solution exists.'), nl, halt.
treatResult(20,BestModel):-
    nl,nl, costOfThisSolution(BestModel,Cost), write('Unsatisfiable. So the optimal solution was this one with cost '),
    write(Cost), write(':'), nl, displaySol(BestModel), halt.
treatResult(10,_):- %   shell('cat model',_),
    write('Solution found '), flush_output,
    see(model), symbolicModel(M), seen,
    costOfThisSolution(M,Cost),
    write('with cost '), write(Cost), nl,nl,
    displaySol(M), 
    Cost1 is Cost-1,   nl,nl,nl,nl,nl,  write('Now looking for solution with cost '), write(Cost1), write('...'), nl,
    initClauseGeneration, tell(clauses), writeClauses(Cost1), told,
    tell(header),  writeHeader,  told,
    numVars(N),numClauses(C),
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result,M),!.
treatResult(_,_):- write('cnf input error. Wrote something strange in your cnf?'), nl,nl, halt.
    

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


