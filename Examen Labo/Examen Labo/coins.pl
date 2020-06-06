%% 3 points.
%% Complete/modify this Prolog program (pure Prolog, not CLP) to solve the following problem.
%% Given a list of types of coins, and a number N, what is the smallest number of coins needed to pay N?

main:- pay( [1,2,5,10,20,50,100,200], 359 ),  % <-- this is the type of coins we have with Euros
       pay( [1,5,6],                   10 ),  % <-- note: starting with the largest coin ("greedy") does not work!
       pay( [1,2,5,13,17,35,157],     361 ).     

%% The output should look like this:
%%  
%%    Paying 359
%%    using coins [1,2,5,10,20,50,100,200]
%%    needs 6 coins:    1*200+1*100+1*50+1*5+2*2
%%  
%%    Paying 10
%%    using coins [1,5,6]
%%    needs 2 coins:    2*5
%%  
%%    Paying 361
%%    using coins [1,2,5,13,17,35,157]
%%    needs 5 coins:    2*157+2*17+1*13
%%
%% Hint: first try to make output that is correct but looks ugly like this (it can give you up to 2 points):
%%    needs 6 coins:    0+1*200+1*100+1*50+0*20+0*10+1*5+2*2+0*1

pay(L,N):-
       write('Paying '), write(N), nl, write('using coins '), write(L),  nl,
       nat(K),
       coins(K,L,N,E),   % E is the expression that uses K coins of the list L to pay the amount N
	write('needs '), write(K), write(' coins:    '), write(E), nl,nl,!.

% Generar los numeros naturales en orden.
nat(0).
nat(N) :- nat(N0), N is N0 + 1.

coins(_,_,0,_):- !, false.
coins(0,_,_,_):- !, false.

coins( K, [C|_], N, K*C):-
       K>=0,
       N>=0,
       N is K*C.

coins( K, [_|Cs], N, E):-
       K>=0,
       N>=0,
       coins(K,Cs,N,E).

coins( K, [C|Cs], N, E + I*C):-
       K>=0,
       N>=0,
       between(1,K,I),       % use I coins of type C
       NewK is K - I,
       NewN is N - I*C,
       coins(NewK,Cs,NewN,E).
