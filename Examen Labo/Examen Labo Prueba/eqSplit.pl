%% Write a Prolog predicate eqSplit(L,S1,S2) that, given a list of
%% integers L, splits it into two disjoint subsets S1 and S2 such that
%% the sum of the numbers in S1 is equal to the sum of S2. It should
%% behave as follows:
%%
%% ?- 
%%
%% [1,5,2,3]    [4,7]
%% [1,3,7]    [5,2,4]
%% [5,2,4]    [1,3,7]
%% [4,7]    [1,5,2,3]



main:- eqSplit([1,5,2,3,4,7],S1,S2), write(S1), write('    '), write(S2), nl, fail.
main.

repartir([],[],[]).
repartir([X|XS],[X|S1],S2):- repartir(XS,S1,S2).
repartir([X|XS],S1,[X|S2]):- repartir(XS,S1,S2).

eqSplit(L,S1,S2):- repartir(L,S1,S2), sum_list(S1, N), sum_list(S2, N).
