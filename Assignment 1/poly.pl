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
    Y > 1, !.
take_power(_*M, M) :- power(M).

% A constant to multiply a monommial.
coefficient(K) :- number(K).
take_coefficient(K*M, K) :- monomial(M), number(K).

% Single term polynomial. Can be one of the forms:
%  - x
%  - 7
%  - x^2
%  - 7*x^2
monomial(X)   :- pvar(X),   !.
monomial(X)   :- power(X),  !.
monomial(N)   :- number(N), !.
monomial(K*X) :- number(K), power(X), !.

% Multiply a monomial by a constant.
mulKmon(K, M, M3) :-
    monomial(M),
    take_coefficient(M, K2),
    take_power(M, M2),
    K3 is K * K2,
    M3 = K3*M2.

% Simplify a monomial.
% - 1*x^k   = x^k 
% - 0*_     = 0
% - a*x^k   = a*x^k
simmon(1*M, M) :- power(M), !.
simmon(0*_, 0) :- !. 
simmon(M, M).

% A polynomial as a sequence of monomials.  
polynomial(M)   :- monomial(M).
polynomial(P+M) :- monomial(M), polynomial(P).
polynomial(P-M) :- monomial(M), polynomial(P).

% Simplify a polynomial.
simpoly(M,M2) :- monomial(M), simmon(M,M2), !.