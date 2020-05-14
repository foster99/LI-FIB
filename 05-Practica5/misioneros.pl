
camino(E,E,C,C).

camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    unPaso( EstadoActual, EstSiguiente ),
    \+member(EstSiguiente,CaminoHastaAhora),
    camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).

solucionOptima:-
    nat(N),                             %  Buscamos solución de "coste" 0; si no, de 1, etc.
    camino([[3,3],[0,0],[0,0],1],[[0,0],[0,0],[3,3],3],[[3,3],[0,0],[0,0],1],C),      %  En "hacer aguas": -un estado es [cubo5,cubo8], y
    length(C,N),                        %  -el coste es la longitud de C.
    write(C).

% Generar los numeros naturales en orden.
nat(0).
nat(N) :- nat(N0), N is N0 + 1.


% Estado: [[M1,C1],[M2,C2],[M3,C3]] === [Lado1, canoa, Lado3]
    % M === #misioneros
    % C === #canibales
    % L === Lado donde esta la canoa

hayHueco(M,C) :- Canoa is M + C, between(0, 1, Canoa).

% mover canoa de lado
unPaso([[M1,C1],[M,C],[M3,C3],1],[[M1,C1],[M,C],[M3,C3],3]) :- Canoa is M + C, between(1, 2, Canoa).
unPaso([[M1,C1],[M,C],[M3,C3],3],[[M1,C1],[M,C],[M3,C3],1]) :- Canoa is M + C, between(1, 2, Canoa).

% subir de 1 a 2
unPaso([[M1,C1],[M2,C2],[M3,C3],1],[[M1_next,C1],[M2_next,C2],[M3,C3],1]) :- % poner 1 misionero en la barca
    hayHueco(M2,C2),
    M1 >= 1,
    M1_next is M1 - 1,
    M2_next is M2 + 1,
    M1_next >= C1.

unPaso([[M1,C1],[M2,C2],[M3,C3],1],[[M1,C1_next],[M2,C2_next],[M3,C3],1]) :- % poner 1 canibal en la barca
    hayHueco(M2,C2),
    C1 >= 1,
    C1_next is C1 - 1,
    C2_next is C2 + 1,
    M1 >= C1_next.

% subir de 3 a 2
unPaso([[M1,C1],[M2,C2],[M3,C3],3],[[M1,C1],[M2_next,C2],[M3_next,C3],3]) :-  % poner 1 misionero en la barca
    hayHueco(M2,C2),
    M3 >= 1,
    M3_next is M3 - 1,
    M2_next is M2 + 1,
    M3_next >= C3.

unPaso([[M1,C1],[M2,C2],[M3,C3],3],[[M1,C1],[M2,C2_next],[M3,C3_next],3]) :-  % poner 1 canibal en la barca
    hayHueco(M2,C2),
    C3 >= 1,
    C3_next is C3 - 1,
    C2_next is C2 + 1,
    M3 >= C3_next.

% bajar de 2 a 1
unPaso([[M1,C1],[M2,C2],[M3,C3],1],[[M1_next,C1],[M2_next,C2],[M3,C3],1]) :- % bajar 1 misionero de la barca
    M2 >= 1,
    M1_next is M1 + 1,
    M2_next is M2 - 1,
    M1_next >= C1.

unPaso([[M1,C1],[M2,C2],[M3,C3],1],[[M1,C1_next],[M2,C2_next],[M3,C3],1]) :- % bajar 1 canibal de la barca
    C2 >= 1,
    C1_next is C1 + 1,
    C2_next is C2 - 1,
    M1 >= C1_next.

% bajar de 2 a 3
unPaso([[M1,C1],[M2,C2],[M3,C3],3],[[M1,C1],[M2_next,C2],[M3_next,C3],3]) :-  % bajar 1 misionero de la barca
    M2 >= 1,
    M3_next is M3 + 1,
    M2_next is M2 - 1,
    M3_next >= C3.

unPaso([[M1,C1],[M2,C2],[M3,C3],3],[[M1,C1],[M2,C2_next],[M3,C3_next],3]) :-  % bajar 1 canibal de la barca
    C2 >= 1,
    C3_next is C3 + 1,
    C2_next is C2 - 1,
    M3 >= C3_next.


