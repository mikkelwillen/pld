machine(x86).
machine(arm).
compiler(c,arm,arm).
compiler(haskell,c,haskell).
interpreter(ml,x86).
interpreter(haskell,c).
program(ml).
program(haskell).

writtenIn(M,program(M)) :-
  program(M).

writtenIn(M,compiler(X, M, Y)) :-
  compiler(X, M, Y).

writtenIn(M,interpreter(X,M)) :-
  interpreter(X,M).


canRun(L) :-
  machine(L).

canRun(L) :-
  interpreter(L,X),
  canRun(X).

canRun(L) :-
  compiler(L,X,Y),
  L \= Y,
  canRun(X),
  canRun(Y).
