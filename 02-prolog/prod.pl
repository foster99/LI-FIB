/* Ejercicio 2 */

prod([],P) :- P is 1.
prod([X|XS], P):- prod(XS, XP), P is X*XP.
