% "append"
concat([], L, L).
concat([X|L1], L2, [X|L3]):- concat(L1, L2, L3).

myLast([], _).
myLast(L, X) :- concat(_,[X], L).

myReverse([],[]).
myReverse([X],[X]).
myReverse([X|L],R) :- concat(R1,[X],R), myReverse(L,R1).