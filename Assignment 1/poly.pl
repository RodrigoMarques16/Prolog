% Logic Programming - Assignment 1
%    FCUP - Fall 2018
%
% Symbollic Manipulation
%

%:-include("syntax.pl") - included in mono.pl
:-include("mono.pl").

%% polynomial(P)
%
% Is true if P is a Monomial or if P is a sum of 
% monomials
%
polynomial(M)   :- monomial(M).
polynomial(P+M) :- monomial(M), polynomial(P), !.
polynomial(M+P) :- monomial(M), polynomial(P), !.
polynomial(P-M) :- monomial(M), polynomial(P), !.

%% poly2list(P, L)
%
% Transforms a list representing a polynomial and vice-versa.
%
poly2list(0, [])  :- !.
poly2list(M, [M]) :- monomial(M), !.

poly2list(P+M, L2) :-
    monomial(M),
    poly2list(P, L),
    append(L, [M], L2), 
    !.

poly2list(M+P, [M|L]) :- 
    monomial(M), 
    poly2list(P, L), 
    !.

%% sort_poly_list(P, P2)
%
% Sorts the polynomial represented as a list. 
% Merges monomials with the same power.
%

sort_poly_list(P, P2) :- 
    predsort(mono_compare, P, P2), !.

%% sort_poly(P, P2)
%
% Sorts the given polynomial in P by converting it to a list
% and calling predicate sort_poly_list/2.
%
sort_poly(P, P2) :-
    poly2list(P, L),
    sort_poly_list(L, L2),
    poly2list(P2, L2), !.

%% compress_poly_list(P, P2)
% 
% Sum together all monomials with the same var power in a
% polynomial.
%
compress_poly_list(P, P3) :-
    sort_poly_list(P, P2),
    compress_poly_list_aux(P2, P3), !.

compress_poly_list_aux([], []).
compress_poly_list_aux([M], [M]). 
compress_poly_list_aux([M1,M2|P], P2) :-
    same_power(M1, M2),
    addmono(M1, M2, M),
    compress_poly_list([M|P], P2).

compress_poly_list_aux([M|P], [M|P2]) :-
    compress_poly_list(P, P2). 

%% compress_poly(P, P2)
%
% Sum together all monomials with the same var power in a
% polynomial.
%
compress_poly(P, P2) :-
    poly2list(P, L),
    compress_poly_list(L, L2),
    poly2list(P2, L2).   

%% simpoly_list(P, P2)
% 
% Simplify a polynomial represented as a list of monomials
%
simpoly_list([], []).

simpoly_list([M], [M2]) :- 
    normalize_mono(M, N), 
    simmono(N, M2), !.

simpoly_list([M|P], L) :-
    normalize_mono(M, N),
    simmono(N, M2),
    simpoly_list(P, P2),
    (   M2 == 0 -> 
        L = P2
    ;   L = [M2|P2]), !.
    
%% simpoly(P, P2)
%
% Simplify a polynomial.
%
simpoly(P+0, P2) :- simpoly(P, P2), !.
simpoly(0+P, P2) :- monomial(P),    simmono(P, P2), !.
simpoly(P, P2) :-
    poly2list(P, L),
    sort_poly_list(L, L2),
    simpoly_list(L2, S),
    poly2list(P2, S), !.

%% scalepoly(P1, K, P2)
%
% Is true if P2 is the polymonial P2 scaled by K
%
scalepoly(M1, K, M2) :-
    monomial(M1),
    scale_mono(M1, K, M2),
    !.

scalepoly(P1+M1, K, P3) :-
    scalepoly(P1, K, P2),
    scale_mono(M1, K, M2), 
    simpoly(P2+M2, P3), 
    !.

% addpoly(P1, P2, P)
%
% Add two polynomials together
%
addpoly(M1, M2, M3) :-
    monomial(M1),
    addmono(M1, M2, M3), !.
addpoly(P1+M1, M2, P1+M3) :-
    addmono(M1, M2, M3), !.




