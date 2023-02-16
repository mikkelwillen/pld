isDictOf(eqInt, eq(int)).
isDictOf(ordInt, ord(int)).

isDictOf(ordBool(X), ord(bool)) :-
  isDictOf(X,ord(int)).

isDictOf(optimizedOrd, ord(prod(int, int))).

isDictOf(ordPair(X,Y), ord(prod(A,B))) :-
  isDictOf(X, ord(A)),
  isDictOf(Y, ord(B)).

isTypeOf(qsortPrime(D,L), list(A)) :-
  isDictOf(D, ord(A)),
  isTypeOf(L, list(A)).

isTypeOf(list(A),list(A)).

isTypeOf(true,bool).
isTypeOf(false,bool).

isTypeOf(int,int).

isTypeOf(X,B) :-
  isSubtypeOf(A,B),
  isTypeOf(X,A).

isSubtypeOf(bool,int).

isSubtypeOf(list(A),list(B)) :-
  isSubtypeOf(A,B).

/*
isSubtypeOf(A,C) :-
  isSubtypeOf(A,B),
  isSubtypeOf(B,C).
*/
