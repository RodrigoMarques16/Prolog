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
polynomial(P+M) :- monomial(M), polynomial(P).
%polynomial(M+P) :- monomial(M), polynomial(P).
polynomial(P-M) :- monomial(M), polynomial(P).

%% poly2list(P, L)
%
% Transforms a list representing a polynomial and vice-versa.
%
poly2list(0, [])  :- !.
poly2list(M, [M]) :- monomial(M), !.

poly2list(P-M, [M2|L]) :-
    monomial(M),
    scale_mono(M, -1, M2),
    poly2list(P, L), 
    !.

poly2list(P+M, [M|L]) :-
    monomial(M),
    poly2list(P, L), 
    !.

/*
poly2list(P+M, L2) :-
    monomial(M),
    poly2list(P, L),
    append(L, [M], L2), 
    !.

poly2list(P-M, L2) :-
    monomial(M),
    poly2list(P, L),
    append(L, [-M], L2), 
    !.

poly2list(M+P, [M|L]) :- 
    monomial(M), 
    poly2list(P, L), 
    !.
*/

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
    poly2list(P2, L2), 
    !.

%% compress_poly_list(P, P2)
% 
% Sum together all monomials with the same var power in a
% polynomial.
%
compress_poly_list(P, P3) :-
    sort_poly_list(P, P2),
    compress_poly_list_aux(P2, P3), 
    !.

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
% Simplify a polynomial represented as a list of monomials.
%
simpoly_list(P1, P) :-
    compress_poly_list(P1, P2),
    simpoly_list_aux(P2, P).

simpoly_list_aux([], []).

simpoly_list_aux([M], [M2]) :- 
    normalize_mono(M, N), 
    simmono(N, M2), 
    !.

simpoly_list_aux([M|P], L) :-
    normalize_mono(M, N),
    simmono(N, M2),
    simpoly_list_aux(P, P2),
    (   M2 == 0 -> 
        L = P2  
    ;   L = [M2|P2]),
    !.
    
%% simpoly(P, P2)
%
% Simplify a polynomial.
%
simpoly(P, P2) :-
    poly2list(P, L),
    sort_poly_list(L, L2),
    simpoly_list(L2, S), 
    poly2list(P2, S).

%% scalepoly_list(P1, K, P2)
%
% Is true if P2 is the polymonial P2 scaled by K
%
scale_poly_list(P1, K, P) :-
    scale_poly_list_aux(P1, K, P2),
    simpoly_list(P2, P).

scale_poly_list_aux([M1], K, [M2]) :-
    scale_mono(M1, K, M2), 
    !.

scale_poly_list_aux([M1|P1], K, [M2|P2]) :-
    scale_poly_list_aux(P1, K, P2), 
    scale_mono(M1, K, M2),
    !.

%% scalepoly(P1, K, P2)
%
% Is true if P2 is the polymonial P2 scaled by K
%
scalepoly(P1, K, P) :-
    poly2list(P1, L),
    scale_poly_list(L, K, P).

% add_poly_list(P1, P2, P)
%
% Add two polynomials together
%
add_poly_list(P1, P2, P) :-
    sort_poly_list(P1, SP1),
    sort_poly_list(P2, SP2),
    add_poly_list_aux(SP1, SP2, P3), 
    !,
    simpoly_list(P3, P).

add_poly_list_aux(P, [], P).
add_poly_list_aux([], P, P).

add_poly_list_aux([M1], [M2], P) :-
    addmono(M1, M2, M),
    poly2list(M, P).

add_poly_list_aux([M1|P1], [M2|P2], P) :-
    same_power(M1, M2),
    addmono(M1, M2, M3),
    poly2list(M3, M),
    add_poly_list_aux([M|P1], P2, P).

add_poly_list_aux([M1|P1], [M2|P2], [M1|P]) :-
    mono_compare(<, M1, M2),
    add_poly_list_aux(P1, [M2|P2], P).

add_poly_list_aux([M1|P1], [M2|P2], [M2|P]) :-
    mono_compare(>, M1, M2),
    add_poly_list_aux([M1|P1], P2, P).

% addpoly(P1, P2, P)
%
% Add two polynomials together
%
addpoly(P1, P2, P) :-
    poly2list(P1, L1),
    poly2list(P2, L2),
    add_poly_list(L1, L2, L), 
    poly2list(P, L).
