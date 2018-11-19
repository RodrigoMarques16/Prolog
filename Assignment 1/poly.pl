% Logic Programming - Assignment 1
% FCUP - Fall 2018
%
% Symbollic Manipulation

% The variables accepted by the program
pvars([x,y,z]).

%% pvar(+X)
%
% True if X is a variable.
%pvar(X) :- pvars(V), member(X,V).
pvar(X) :- atom(X).

%% power(X^Y)
% True if X is a variable and ^Y is omitted or a non-negative integer
power(X)   :- pvar(X), !.
power(X^Y) :- pvar(X), integer(Y), Y >= 0, !.

%% coefficient(K)
% True if K is a number.
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

% Make sure a monomial is of the form Coefficient * Power ^ Exponent
normalize_mono(M, M2) :-
    monomial(M),
    monparts(M,K,P), 
    M2 = K*P.

% A polynomial as a sum of monomials.  
polynomial(M)   :- monomial(M).
polynomial(P+M) :- monomial(M), polynomial(P).
polynomial(M+P) :- monomial(M), polynomial(P).
polynomial(P-M) :- monomial(M), polynomial(P).

% Extract from monomial
% monparts/3(Monomial, Coefficient, Power)
monparts(X^N, 1, X^N) :- power(X^N), !.
monparts(K*P, K, P)   :- number(K),  !.
monparts(P*K, K, P)   :- number(K),  !.
monparts(K, K, K) :- number(K),  !.
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
multmono(M1, K, M2) :-
    monomial(M1),
    monparts(M1, K1, P1),
    K2 is K * K1,
    M2 = K2 * P1, !.

% Mutiply a polynomial by a constant.
multpoly(M1, K, M2) :-
    monomial(M1),
    multmono(M1, K, M2), !.
multpoly(P1+M1, K, P2+M2) :-
    multpoly(P1, K, P2),
    multmono(M1, K, M2), !.

% Sum two polynomials
sumpoly(M1, M2, M3) :-
    monomial(M1),
    summono(M1, M2, M3), !.
sumpoly(P1+M1, M2, P1+M3) :-
    summono(M1, M2, M3), !.

% Simplify a monomial.
simmono(1*M, M2)    :- power(M),  simmono(M, M2), !.
simmono(0*_, 0)     :- !. 
simmono(K*M^1, K*M) :- number(K), !.
simmono(M^1, M)     :- pvar(M),   !. 
simmono(_^0, 1)     :- !.
simmono(K*_^0, K)   :- number(K), !.
simmono(M, M).



% True if M is the single element of a list [M].
poly2list(M, [M]) :- monomial(M), !.
% Fill a list with the monomials of a polynomial. Right to left.
poly2list(P+M, L2) :-
    monomial(M),
    poly2list(P, L),
    append(L, [M], L2),
    !.
% Construct a polynomial form a list. Left to right.
% Need to fix style: a + (b + (c + d + (...) ) )
poly2list(M+P, [M|L]) :- monomial(M), poly2list(P, L), !.

/*
%% Sort a polymonial by degree and alphabetical order
simpoly_list(L, L3) :-
    sortpoly(L, L2),
    simpoly_list_helper(L2, L3).
*/

%% Simplify a polynomial represented as a list of monomials
% TODO: handle 0's
simpoly_list([], []).
simpoly_list([M], [M2]) :- 
    normalize_mono(M, N), 
    simmono(N, M2), !.
simpoly_list([M|P], [M2|P2]) :-
    normalize_mono(M, N),
    simmono(N, M2),
    simpoly_list(P, P2), !.

% Simplify a polynomial.
%simpoly(P+0, P2)    :- simpoly(P, P2), !.
%simpoly(0+P, P2)    :- monomial(P),    simmono(P, P2), !.
%simpoly(M, M2)      :- monomial(M),    simmono(M, M2), !.
%simpoly(P+M, P2+M2) :- simpoly(P, P2), simmono(M, M2).
simpoly(P, P2) :-
    poly2list(P, L),
    simpoly_list(L, S),
    poly2list(P2, S).