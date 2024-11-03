tablero([
    celda(1,1, hoyo), celda(1, 2, hongo), celda(1, 3, hongo), celda(1, 4, vacio), celda(1, 5, hoyo),
    celda(2,1, vacio), celda(2, 2, vacio), celda(2, 3, vacio), celda(2, 4, hongo), celda(2, 5, vacio),
    celda(3,1, vacio), celda(3, 2, vacio), celda(3, 3, hoyo), celda(3, 4, conejo), celda(3, 5, vacio),
    celda(4,1, vacio), celda(4, 2, vacio), celda(4, 3, vacio), celda(4, 4, vacio), celda(4, 5, vacio),
    celda(5,1, hoyo), celda(5, 2, vacio), celda(5, 3, vacio), celda(5, 4, vacio), celda(5, 5, hoyo)
    ]).


actualizar_celda(Y, X, NuevoObjeto, Tablero, NuevaLista) :-
    
    maplist(actualizar_celda_aux(Y, X, NuevoObjeto), Tablero, NuevaLista), !.


actualizar_celda_aux(Y, X, NuevoObjeto, celda(Y, X, _), celda(Y, X, NuevoObjeto)).
actualizar_celda_aux(_, _, _, Celda, Celda).  % Mantiene las celdas que no se actualizan

max(A, B, A) :- A>=B.
max(A, B, B) :- A=<B.
min(A, B, A) :- A=<B.
min(A, B, B) :- A>=B.

obstaculos(X1, Y, X2, Y, Tablero) :-
    nonvar(X1), nonvar(X2),
    min(X1, X2, Xmin),
    max(X1, X2, Xmax),
    Xobs is (Xmax - Xmin - 1),
    findall(Tipo, (member(celda(Y, X, Tipo), Tablero),
                    Tipo \= vacio, 
                    Tipo \= hoyo, 
                    entre(X, X1, X2)), Obstaculo),
    Obstaculo \= [],
    length(Obstaculo, Obs),
    Xobs = Obs,!.

obstaculos(X, Y1, X, Y2, Tablero) :-
    min(Y1, Y2, Xmin),
    max(Y1, Y2, Xmax),
    limiteTablero(X, Y1, X, Y2),
    Xobs is (Xmax - Xmin - 1),
    findall(Tipo, (member(celda(Y, X, Tipo), Tablero),
                    Tipo \= vacio, 
                    Tipo \= hoyo, 
                    entre(Y, Y1, Y2)), Obstaculo),
    Obstaculo \= [],
    length(Obstaculo, Obs),
    Xobs = Obs,!.

obstaculos(_, _, _, _, _) :- fail.

entre(A, B, C) :-
    min(B, C, Min),
    max(B, C, Max),
    A > Min,
    A < Max,!.

limiteTablero(X1, Y1, X2, Y2) :-
    number(X1),
    number(Y1),
    number(X2),
    number(Y2),
    entre(X1, 0, 6),
    entre(Y1, 0, 6),
    entre(X2, 0, 6),
    entre(Y2, 0, 6).

    

solucion(Tablero) :-
    \+ member(celda(_, _, conejo), Tablero).
  
move(Tablero, X1, Y1, X2, Y2, NewTablero) :-
    salto_valido(X1, Y1, X2, Y2, Tablero),
    actualizar_celda(Y2, X2, conejo, Tablero, TableroTemp),
    actualizar_celda(Y1, X1, vacio, TableroTemp, NewTablero).

move(Tablero, X1, Y1, X2, Y2, NewTablero) :-
    member(celda(Y1, X1, conejo), Tablero),
    member(celda(Y2, X2, hoyo), Tablero),
    limiteTablero(X1, Y1, X2, Y2),
    obstaculos(X1, Y1, X2, Y2, Tablero),
    actualizar_celda(Y2, X2, hoyoConejo, Tablero, TableroTemp),
    actualizar_celda(Y1, X1, vacio, TableroTemp, NewTablero).

move(Tablero, X1, Y1, X2, Y2, NewTablero) :-
    member(celda(Y1, X1, hoyoConejo), Tablero),
    member(celda(Y2, X2, vacio), Tablero),
    limiteTablero(X1, Y1, X2, Y2),
    obstaculos(X1, Y1, X2, Y2, Tablero),
    actualizar_celda(Y2, X2, conejo, Tablero, TableroTemp),
    actualizar_celda(Y1, X1, hoyo, TableroTemp, NewTablero).

move(Tablero, X1, Y1, X2, Y2, NewTablero) :-
    (
        XT is X1 - 1, YT is Y1, X2 is X1 + 1, Y2 is Y1;
        XT is X1 + 1, YT is Y1,  X2 is X1 - 1, Y2 is Y1
    ), 
    member(celda(Y1, X1, foxH), Tablero),
    member(celda(YT, XT, foxH), Tablero),
    member(celda(Y2, X2, vacio), Tablero),                  
    limiteTablero(X1, Y1, X2, Y2),
    actualizar_celda(Y2, X2, foxH, Tablero, TableroTemp1),
    actualizar_celda(Y1, X1, foxH, TableroTemp1, TableroTemp2),
    actualizar_celda(YT, XT, vacio, TableroTemp2, NewTablero).

move(Tablero, X1, Y1, X2, Y2, NewTablero) :-
    (
        XT is X1, YT is Y1 - 1, X2 is X1, Y2 is Y1 + 1;                 
        XT is X1, YT is Y1 + 1 ,  X2 is X1, Y2 is Y1 - 1
    ),
    member(celda(Y1, X1, foxV), Tablero),
    member(celda(YT, XT, foxV), Tablero),
    member(celda(Y2, X2, vacio), Tablero),
    limiteTablero(X1, Y1, X2, Y2),
    actualizar_celda(Y2, X2, foxV, Tablero, TableroTemp1),
    actualizar_celda(Y1, X1, foxV, TableroTemp1, TableroTemp2),
    actualizar_celda(YT, XT, vacio, TableroTemp2, NewTablero).
    
salto_valido(X1, Y1, X2, Y2, Tablero) :-
    member(celda(Y1, X1, conejo), Tablero),
    member(celda(Y2, X2, vacio), Tablero),
    limiteTablero(X1, Y1, X2, Y2),
    obstaculos(X1, Y1, X2, Y2, Tablero).


prueba(X1, Y1, X2, Y2, NewTablero) :-
    tablero(Tablero),
    move(Tablero, X1, Y1, X2, Y2, NewTablero).

solucionarJuego(Movimientos) :-
    tablero(Tablero),
    resolver(Tablero, [Tablero], Movimientos).

resolver(Tablero, _, []) :-
    solucion(Tablero), !.

resolver(Tablero, Visitados, [Movimiento | Movimientos]) :-         
    move(Tablero, X1, Y1, X2, Y2, NuevoTablero),
    Movimiento = move(X1, Y1, X2, Y2),
    \+ member(NuevoTablero, Visitados),
    resolver(NuevoTablero, [NuevoTablero | Visitados], Movimientos).

resolver(_, _, _) :- fail.