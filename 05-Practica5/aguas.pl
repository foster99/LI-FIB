
camino( E,E, C,C ).

camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    unPaso( EstadoActual, EstSiguiente ),
    \+member(EstSiguiente,CaminoHastaAhora),
    camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).

solucionOptima:-
    nat(N),                             %  Buscamos solución de "coste" 0; si no, de 1, etc.
    camino([0,0],[0,4],[[0,0]],C),      %  En "hacer aguas": -un estado es [cubo5,cubo8], y
    length(C,N),                        %  -el coste es la longitud de C.
    write(C).

% Generar los numeros naturales en orden.
nat(0).
nat(N) :- nat(N0), N is N0 + 1.


unPaso([_, C8], [5, C8]). % llenar cubo 5
unPaso([C5, _], [C5, 8]). % llenar cubo 8
unPaso([_, C8], [0, C8]). % vaciar cubo 5
unPaso([C5, _], [C5, 0]). % vaciar cubo 8

% Pasar de cubo5 a cubo8
unPaso([C5, C8], [C5_next, C8_next]) :-
    Hueco is 8 - C8,
    Hueco > 0,
    AuxC5 is C5 - Hueco,
    C5_next is max(0, AuxC5),
    C8_next is C8 + C5 - C5_next.

% Pasar de cubo8 a cubo5
unPaso([C5, C8], [C5_next, C8_next]) :-
    Hueco is 5 - C5,
    Hueco > 0,
    AuxC8 is C8 - Hueco,
    C8_next is max(0, AuxC8),
    C5_next is C5 + C8 - C8_next.

