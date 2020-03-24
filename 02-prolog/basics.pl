/* Ejercicio 0 */

natural(0).
natural(N):- natural(N1), N1 is N + 1.

fact(0,1).
fact(N, F):- N1 is N-1, fact(N1, F1), F is N*F1.

% "append"
concat([], L, L).
concat([X|L1], L2, [X|L3]):- concat(L1, L2, L3).
% concat(L1, L2, [a,b,c]), write([L1, L2]), nl, fail.


% Factorizacion de un natural
factores(1, []):- !.  % Una vez llega al factor 1, toda otra clausula debe ser invalidada (corte).
factores(N, [F|L]):- natural(F), F > 1, 0 is N mod F, N1 is N // F, factores(N1, L).     % és la divisió d'enters
                    
% Com està implementat el not "\+" -> if (X falla en temps finit) then OK else KO.
not(X):- X, !, fail.
not(_).