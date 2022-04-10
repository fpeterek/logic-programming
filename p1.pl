% fact(N, res)

fact(0, 1).
fact(N, F) :- N>0, N1 is N-1, fact(N1, F1), F is F1*N.

fib(1, 0).
fib(2, 1).
fib(N, F) :- N>0, N1 is N-1, N2 is N-2, fib(N1, F1), fib(N2, F2), F is F1+F2.

nat(0).
nat(N) :- N>0, N1 is N-1, nat(N1).

gen(0).
gen(N) :- gen(N1), N is N1+1 .

edge(a,b).
edge(b,c).
edge(c,d).
edge(a,e).
edge(e,d).

path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

