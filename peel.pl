ptree(X) :- atomic(X), print(X).
ptree(X) :- X =.. [F,L,R], ptree(L),print(F), ptree(R).



