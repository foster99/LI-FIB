/* Ejercicio 4 */

intersec([],_,[]).
intersec([X|XS],YS,[X|I]) :- member(X,YS), !, intersec(XS,YS,I). /* remove_element(X,YS,YAUX) */
intersec([_|XS],YS,I):- intersec(XS,YS,I). /* not member no hace falta, porque la anterior tiene corte */
