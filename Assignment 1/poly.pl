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

/*
poly2list(P, [M1|[M2|L]]) :-
    addmono(M1,M2,P1),
    poly2list(P2, L),
    addpoly(P1, P2,P), 
    !.
*/

%% scalepoly(P1, K, P2)
%
% Is true if P2 is the polymonial P2 scaled by K
%
scalepoly(M1, K, M2) :-
    monomial(M1),
    scale_mono(M1, K, M2),
    %simmono(M2, M3), 
    !.

/*
scalepoly(P1+M1, K, P3) :-
    scalepoly(P1, K, P2),
    scale_mono(M1, K, M2), 
    simpoly(P2+M2, P3), 
    !.
*/
% Sum two polynomials
addpoly(M1, M2, M3) :-
    monomial(M1),
    addmono(M1, M2, M3), !.
addpoly(P1+M1, M2, P1+M3) :-
    addmono(M1, M2, M3), !.


/*
%% Sort a polynomial by degree and alphabetical order
simpoly_list(L, L3) :-
    sortpoly(L, L2),
    simpoly_list_helper(L2, L3).
*/

%% Simplify a polynomial represented as a list of monomials
% TODO: sort, handle repeats
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
    
% Simplify a polynomial.
simpoly(P+0, P2) :- simpoly(P, P2), !.
simpoly(0+P, P2) :- monomial(P),    simmono(P, P2), !.
simpoly(P, P2) :-
    poly2list(P, L),
    sortpoly_list(L, L2),
    simpoly_list(L2, S),
    poly2list(P2, S), !.


% Variable comparator
var_compare(<, indep, _).
var_compare(>, _, indep).
var_compare(<, X, Y) :- X @< Y.
var_compare(>, X, Y) :- X @> Y.

% Monomial comparator
mono_compare(Comp, M1, M2) :-
    monparts(M1, _, Var1^Exp1),
    monparts(M2, _, Var2^Exp2),
    Var1=Var2,
    compare(Comp, Exp1, Exp2).

mono_compare(Comp, M1, M2) :-
    monparts(M1, _, Var1^_),
    monparts(M2, _, Var2^_),
    var_compare(Comp, Var1, Var2).

% Avoid predsort removing equal monomials.
compare_no_equal(<, M1, M2) :- mono_compare(<, M1, M2), !.
compare_no_equal(<, M1, M2) :- mono_compare(=, M1, M2), !.
compare_no_equal(>, M1, M2) :- mono_compare(>, M1, M2), !.

% Sort a polynomial represented as a list
sortpoly_list(P, P2) :- 
    predsort(compare_no_equal, P, P2), !.

% Sort a polymonial
sortpoly(P, P2) :-
    poly2list(P, L),
    simpoly_list(L, L2),
    sortpoly_list(L2, L3),
    poly2list(P2, L3), !.