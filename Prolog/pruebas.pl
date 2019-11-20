qsort([ ], [ ]).
qsort([X|Xs], S) :- menores(X,Xs,L1), mayores(X,Xs,L2),
qsort(L1,S1), qsort(L2,S2), append(S1,[X|S2],S).


menores(_,[ ],[ ]).
menores(X,[Y|Ys],[X|Xs]) :- X < Y, menores(X,Ys,Xs).
menores(X,[Y|Ys],Xs) :- X > Y, menores(X,Ys,Xs).

mayores(_,[ ],[ ]).
mayores(X,[Y|Ys],[X|Xs]) :- X > Y, mayores(X,Ys,Xs).
mayores(X,[Y|Ys],Xs) :- X < Y, mayores(X,Ys,Xs).




