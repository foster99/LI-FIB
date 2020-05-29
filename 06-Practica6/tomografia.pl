% A matrix which contains zeroes and ones gets "x-rayed" vertically and
% horizontally, giving the total number of ones in each row and column.
% The problem is to reconstruct the contents of the matrix from this
% information. Sample run:
%
%	?- p.
%	    0 0 7 1 6 3 4 5 2 7 0 0
%	 0                         
%	 0                         
%	 8      * * * * * * * *    
%	 2      *             *    
%	 6      *   * * * *   *    
%	 4      *   *     *   *    
%	 5      *   *   * *   *    
%	 3      *   *         *    
%	 7      *   * * * * * *    
%	 0                         
%	 0                         
%	

:- use_module(library(clpfd)).

ejemplo1( [0,0,8,2,6,4,5,3,7,0,0], [0,0,7,1,6,3,4,5,2,7,0,0] ).
ejemplo2( [10,4,8,5,6], [5,3,4,0,5,0,5,2,2,0,1,5,1] ).
ejemplo3( [11,5,4], [3,2,3,1,1,1,1,2,3,2,1] ).

p :-	
	ejemplo1(RowSums,ColSums),

    %% Variables
	length(RowSums,N),
	length(ColSums,M),
	NVars is N*M,
	listVars(NVars,L),  % generate a list of Prolog vars (their names do not matter)

    %% Dominio
	L ins 0..1,
	matrixByRows(L,M,MatrixByRows),
	transpose(MatrixByRows, MatrixByColumns),

	%% 2. constraints
	sumListConstraints(MatrixByRows,RowSums),
	sumListConstraints(MatrixByColumns,ColSums),

    %% 3. labeling
	label(L),
	
    %% 4. write
	pretty_print(RowSums,ColSums,MatrixByRows).


listVars(0,[]).
listVars(NVars, [_|L]):- NVarsNext is NVars - 1, listVars(NVarsNext, L).


matrixByRows(L,M,Mat):- length(L,N), N =< M, Mat = [L], !.
matrixByRows(L,M,Mat):- append(L1,L2,L), length(L1,M), matrixByRows(L2,M,LNext), append([L1], LNext, Mat).


sumListConstraints([],[]):- !.
sumListConstraints([X|XS],[Y|YS]):- sum(X, #=, Y), sumListConstraints(XS,YS).


pretty_print(_,ColSums,_):- write('     '), member(S,ColSums), writef('%2r ',[S]), fail.
pretty_print(RowSums,_,M):- nl,nth1(N,M,Row), nth1(N,RowSums,S), nl, writef('%3r   ',[S]), member(B,Row), wbit(B), fail.
pretty_print(_,_,_).


wbit(1):- write('*  '),!.
wbit(0):- write('   '),!.
    