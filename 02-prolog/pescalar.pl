/* Ejercicio 3 */

pescalar([X],[Y],P) :- P is X*Y.
pescalar([X|XS],[Y|YS],P) :- pescalar(XS,YS,PP), P is X*Y + PP.
