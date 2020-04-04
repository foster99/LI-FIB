todosClaseTodosProfe:-
    numAssignatures(N),
    assig(_,C,_,_,_),
    course(C),
    findall( cr(C,R), room(R), Lits1),
    exactly(N,Lits1),
    findall( ct(C,T), teacher(T), Lits2),
    exactly(N,Lits2),
    fail.
todosClaseTodosProfe.