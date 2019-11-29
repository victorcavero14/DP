/*
3ºA Programación declarativa.
Alumno : Víctor Manuel Cavero Gracia


-----Ejercicio 1:

?- profesion(X,Y), quizas_tenga(Y,taladro).
    X = roberto 
    Y = carpintero.

?- sospecho(X,Y,relacion_sentimental).
    MAL ESCRITO Sería sospechoso(X,Y,relacion_sentimental).
    X = alfonso
    Y = filemona.

?- asesinado_con(X,Y), sospecho(Z,Y,_).
    false.

?- profesion(X,Y), quizas_tenga(Y,Z), es(Z,arma_blanca).
    X = alfonso,
    Y = carnicero,
    Z = cuchillo ;
    X = roberto,
    Y = carpintero,
    Z = sierra ;
    false.

?- tiene_pasado_turbio(X), sospechoso(X,_,_).

    X = juan ;
    X = juan ;
    false.
    
    
-----
Los predicados formados con hechos simples no tienen mucho que explicar, 
se hacen true cuando se cumple exactamente la situación.

Ejemplo: 

tiene_pasado_turbio(juan).
profesion(juan,informatico).
es(cuchillo, arma_blanca).
asesinado_con(filomena, cuchillo).

Y los ultimos predicados que a su vez estan formados por otros predicados indican
si alguien es sospechoso en funcion de si cumple que o tuvo relacion sentimental o tiene
pasado turbio o por su profesion tiene un tipo de objeto.

Añadiendo :

tuvo_relacion_sentimental(luis,rosa).

El objetivo :

?- sospechoso(X,Y, relacion_sentimental).
    X = luis,
    Y = rosa ;
    X = alfonso,
    Y = filomena.

*/

% -----Ejercicio 2:

mezcla(_, [], []).
mezcla([], _, []).
mezcla([X1|L1],[X2|L2], [X1,X2|L]) :- mezcla(L1, L2, L).

% -----Ejercicio 3:

% a) L1 es sublista de L2 (con todos elems consecutivos)

unir( [], L2, L2).               
unir( [X1 | L1], L2, [X1 | L3]) :- unir( L1, L2, L3). 

sublista(L1, L2):-
    unir(_, L3, L2),
    unir(L1, _, L3).

% b) L1 contenida en L2 (no necesario elem consecutivos)

%contiene([X1 | L1], [X1 | L2]). 
%contiene([X1 | L1], [X2 | L2]) :- 

% -----Ejercicio 4:

% Calcular el numero de nodos de un arbol binario (con aritmetica de Peano)

