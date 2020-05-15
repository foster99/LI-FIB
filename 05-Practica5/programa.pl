
programa(L) :- append([['begin'],L1,['end']], L), instrucciones(L1).

instrucciones(L):- append([I,[';'],L1],L), instruccion(I), instrucciones(L1).
instrucciones(I):- instruccion(I).

instruccion([Var1,'=',Var2,'+',Var3]) :- variable(Var1), variable(Var2), variable(Var3).
instruccion(L) :-
    append([['if',Var1,'=',Var2,'then'],L1,['else'],L2,['endif']],L),
    variable(Var1), variable(Var2), instrucciones(L1), instrucciones(L2).


variable('x').
variable('y').
variable('z').


test :- 
    programa(['begin', 'z', '=', 'x', +, 'y', 'end']),
    programa(['begin', 'z', '=', 'x', +, 'y', ;, 'z', =, 'z', +, 'y', ;, 'y', =, 'y', +, 'y', 'end']),
    programa(['begin', 'z', '=', 'x', +, 'y', ;, 'if', 'x', =, 'y', 'then', 'z', =, 'z', +, 'y', ;, 'y', =, 'y', +, 'y', 'else', 'z', =, 'x', +, 'y', ;, 'z', =, 'z', +, 'y', 'endif', end]),
    programa(['begin', 'z', '=', 'x', +, 'y', ;, 'if', 'x', =, 'y', 'then', 'if', 'x', =, 'y', 'then', 'z', =, 'z', +, 'y', ;, 'y', =, 'y', +, 'y', 'else', 'z', =, 'x', +, 'y', ;, 'z', =, 'z', +, 'y', 'endif', ';', 'z', =, 'z', +, 'y', ;, 'y', =, 'y', +, 'y', 'else', 'z', =, 'x', +, 'y', ;, 'z', =, 'z', +, 'y', 'endif', end]),
    not(programa(['begin', 'z', =, 'x', +, 'y', ;, 'x', =, 'z', 'z', 'end'])).