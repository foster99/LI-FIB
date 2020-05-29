p :-M = 1, permutation([S,E,N,D,O,R,Y,_,_], [0,2,3,4,5,6,7,8,9]),
    write([S,E,N,D] + [1,O,R,E] = [M,O,N,E,Y]), nl,
    R is 1000*(S) + 100*(E+O) + 10*(N+R) + (D+E),
    R is 10000 + 1000*O + 100*N + 10*E + Y,
    write('yay'),
    write([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]), nl, halt.

    %  SEND
    %  MORE
    % MONEY
