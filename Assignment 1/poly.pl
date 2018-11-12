% Logic Programming - Assignment 1
% FCUP - Fall 2018
%
% Symbollic Manipulation

% The variables accepted by the program
pvars([x,y,z]).

% True if X is a variable
pvar(X) :- pvars(V), member(X,V).

% A variable to the power of a positive integer (1 by omission)
%  - x
%  - x^y
power(X)   :- pvar(X), !.
power(X^Y) :- 
    pvar(X), 
    integer(Y), 
    Y >= 0, !.

% A constant to multiply a monommial.
coefficient(K) :- number(K).

% Single term polynomial. Can be one of the forms:
%  - x
%  - x^2
%  - 7
%  - 7 * x^2
%  - x^2 * 7
monomial(X)   :- pvar(X),   !.
monomial(X)   :- power(X),  !.
monomial(N)   :- number(N), !.
monomial(K*X) :- number(K), power(X), !.
monomial(X*K) :- number(K), power(X), !.

% A polynomial as a sum of monomials.  
polynomial(M)   :- monomial(M).
polynomial(P+M) :- monomial(M), polynomial(P).
polynomial(P-M) :- monomial(M), polynomial(P).

% Extract from monomial
% monparts/3(Monomial, Coefficient, Power)
monparts(X^N, 1, X^N) :- power(X^N), !.
monparts(K*P, K, P)   :- number(K),  !.
monparts(K, K, indep) :- number(K),  !.
monparts(X, 1, X)     :- pvar(X),    !.

% Sum two monomials with the same power
summono(M1, M2, M3) :-
    monparts(M1, K1, P1),
    monparts(M2, K2, P1),
    K3 is K1+K2,
    M3 = K3*P1, !.

% Sum two monomials with different powers
summono(M1, M2, M3) :-
    monparts(M1, _, P1),
    monparts(M2, _, P2),
    P1 \== P2,
    M3 = M1+M2, !.

% Multiply a monomial by a constant.
mulKmon(K, M1, M2) :-
    monomial(M1),
    monparts(M1, K1, P1),
    K2 is K * K1,
    M2 = K2 * P1, !.

% Simplify a monomial.
simmon(1*M, M2)    :- power(M),  simmon(M, M2), !.
simmon(0*_, 0)     :- !. 
simmon(K*M^1, K*M) :- number(K), !.
simmon(M^1, M)     :- pvar(M),   !. 
simmon(_^0, 1)     :- !.
simmon(K*_^0, K)   :- number(K), !.
simmon(M, M).

% Simplify a polynomial.
simpoly(M, M2) :- monomial(M), simmon(M,M2), !.