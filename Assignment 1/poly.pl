% Logic Programming - Assignment 1
% FCUP - Fall 2018
%
% Symbollic Manipulation
%
% TODO:
%   - Sort before simplification
%   - Simplify repeated monomials in a polynomial
%   - Negative coefficients


%% pvars(L)
%
% Used to get the list of variables accepted by the 
% program.
%
pvars([x,y,z]).

%% pvar(X)
%
% True if X is a variable accepted by the program.
%
%pvar(X) :- pvars(V), member(X,V), !.
pvar(X) :- atom(X).

%% exponent(E)
%
% True if E is a valid exponent for a monomial.
%
exponent(E) :- integer(E), E >= 0.

%% power(X)
%% power(X^Y)
%
% True if X is a variable and Y is either omitted 
% or a non-negative integer
%
power(V)   :- pvar(V).
power(V^E) :- pvar(V), exponent(E).

%% varpower(VP, V, E)
%
% Extracts parts from a var power.
%
varpower(V, V, 1)   :- pvar(V).
varpower(V^E, V, E) :- pvar(V), exponent(E).

%% coefficient(K)
%
% True if K is a number.
%
coefficient(K) :- number(K).

%% monomial(M)
%
% Is true if M is monomial. A monomial can be:
%   - Single var:              x
%   - Var power:               x^Exp
%   - Coefficient:             Number
%   - Coefficient * Var power: K * x^Exp
%   - Var Power * Coefficient: x^Exp * K
%
monomial(X*K) :- number(K), power(X), !.
monomial(K*X) :- number(K), power(X), !.
monomial(V)   :- pvar(V),   !.
monomial(VP)  :- power(VP), !.
monomial(N)   :- number(N), !.

% monparts(M, K, V^E)
%
% Extract parts from monomial
%
monparts(V, 1, V^1)     :- pvar(V),    !.
monparts(K, K, indep^1) :- number(K),  !.
monparts(V^E, 1, V^E)   :- power(V^E), !.
monparts(K*VP, K, V^E)  :- number(K),  varpower(VP, V, E), !.
monparts(VP*K, K, V^E)  :- number(K),  varpower(VP, V, E), !.

%% normalize_mono(M, M2)
%
% Transform a monomial in the form:
%   - Coefficient * Var ^ Exponent
%
% Reverses simplification.
%
normalize_mono(M, M2) :-
    monomial(M),
    monparts(M,K,VP), 
    M2 = K*VP, !.

%% addmono(M1, M2, M3)
% 
% True if M3 is the sum of M1 with M3.
% If both monomials have the same var power the sum
% results in a polynomial.
%
addmono(M1, M2, M3) :-
    monparts(M1, K1, VP1),
    monparts(M2, K2, VP1),
    K3 is K1+K2,
    M3 = K3*VP1, 
    !.

addmono(M1, M2, M3) :-
    monparts(M1, _, VP1),
    monparts(M2, _, VP2),
    VP1 \== VP2,
    M3 = M1+M2, 
    !.

%% scalemono(M1, K, M2)
%
% True if M2 is M1 multiplied by the scalar K.
%
scale_mono(M1, K, M3) :-
    monomial(M1),
    monparts(M1, K1, VP),
    K2 is K * K1,
    M2 = K2 * VP, 
    simmono(M2, M3), 
    !.

%% simmono(M1, M2)
%
% Transforms M1 into a simplified form.
% Normalizes the monomial before simplifying.
%
simmono(M1, M2) :- 
    normalize_mono(M1, M),
    simmono_aux(M, M2).

simmono_aux(1*VP, M2)   :- simmono(VP, M2), !.
simmono_aux(0*_, 0)     :- !.
simmono_aux(K*V^1, K*V) :- coefficient(K),  !.
simmono_aux(V^1, V)     :- pvar(V),         !. 
simmono_aux(_^0, 1)     :- !.
simmono_aux(K*_^0, K)   :- number(K),       !.
simmono_aux(M, M).

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
poly2list(0, []).
poly2list(M, [M]) :- monomial(M), !.

poly2list(P+M, L2) :-
    monomial(M),
    poly2list(P, L),
    append(L, [M], L2), !.

%% TODO: fix list to poly
%poly2list(M+P, [M|L]) :- 
%    monomial(M), 
%    poly2list(P, L), !.
poly2list(P, [M1|[M2|L]]) :-
    addmono(M1,M2,P1),
    poly2list(P2, L),
    addpoly(P1, P2,P), !.

%% scalepoly(P1, K, P2)
%
% Is true if P2 is the polymonial P2 scaled by K
%
scalepoly(M1, K, M3) :-
    monomial(M1),
    scale_mono(M1, K, M2),
    simmono(M2, M3), 
    !.

scalepoly(P1+M1, K, P3) :-
    scalepoly(P1, K, P2),
    scale_mono(M1, K, M2), 
    simpoly(P2+M2, P3), 
    !.

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