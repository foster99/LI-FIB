

%%% DisplaySol: show the solution. Here M contains the literals that are true in the model:

displaySol(_):- nl,nl, write('Input:   '), coord(X), nl, coord(Y), writeInputSq(X,Y), fail. 
displaySol(M):- nl,nl, write('Solution:'), coord(X), nl, coord(Y),
		member(color(X,Y,Color),M), setColor(Color), write(' o'), fail. 
displaySol(_):- resetColor, !.

writeInputSq(X,Y):- c(Color,X,Y,_,_), setColor(Color), write(' o'), !.
writeInputSq(X,Y):- c(Color,_,_,X,Y), setColor(Color), write(' o'), !.
writeInputSq(_,_):- resetColor, write(' ·'), !.

setColor(Color):- colorCode(Color,Code), put(27), write('[0;38;5;'), write(Code), write('m'), !.
resetColor:- put(27), write('[0m'), !.

colorCode( blue,       69  ).
colorCode( brown,      138 ).
colorCode( red,        196 ).
colorCode( cyan,       51  ).
colorCode( green,      46  ).
colorCode( yellow,     226 ).
colorCode( pink,       201 ).
colorCode( violet,     90  ).
colorCode( orange,     208 ).
colorCode( darkblue,   21  ).
colorCode( darkgreen,  28  ).
colorCode( darkred,    88  ).
colorCode( darkcyan,   30  ).
colorCode( white,      15  ).
colorCode( grey,       8   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
