/* Ejercicio 4 */

intersec([],_,[]).
intersec(_,[],[]).
intersec([X|XS],YS,[X|I]) :- member(X,YS), !, intersec(XS,YS,I). /* remove_element(X,YS,YAUX) */
intersec([_|XS],YS,I):- intersec(XS,YS,I). /* not member no hace falta, porque la anterior tiene corte */


myUnion([],L,L).
myUnion(L,[],L).
myUnion([X|L], L2, L3) :- member(X, L2), !, myUnion(L,L2,L3).
myUnion([X|L], L2, [X|L3]) :- myUnion(L,L2,L3).


/* coger un elemento X de una lista L en posicion desconocida =>  append(L1, [X|L2], L) */
remove_element(X, L, NewL) :- append(L1, [X|L2], L), append(L1,L2,NewL).