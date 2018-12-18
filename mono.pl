:-include("syntax.pl").

%% monomial(M)
%
% Is true if M is monomial. A monomial can be:
%   - Single var:              x
%   - Var power:               x^Exp
%   - Coefficient:             Number
%   - Coefficient * Var power: K * x^Exp
%   - Var Power * Coefficient: x^Exp * K
%
monomial(K*X) :- coefficient(K), power(X).
monomial(X*K) :- coefficient(K), power(X).
monomial(VP)  :- power(VP).
monomial(-VP) :- power(VP).
monomial(N)   :- number(N).
%monomial(-V) :- pvar(V).
%monomial(V)  :- pvar(V).

% monparts(M, K, V^E)
%
% Extract parts from monomial
%
monparts(V, 1, V^1)             :- pvar(V).
monparts(-V, -1, V^1)           :- pvar(V).
monparts(V^E, 1, V^E)           :- power(V^E).
monparts(-V^E, -1, V^E)         :- power(V^E).
monparts(K, K, indep^1)         :- coefficient(K).
monparts(K*indep, K, indep^1)   :- coefficient(K).
monparts(K*indep^1, K, indep^1) :- coefficient(K).
monparts(K*VP, K, V^E)          :- coefficient(K), varpower(VP, V, E).
monparts(VP*K, K, V^E)          :- coefficient(K), varpower(VP, V, E).

%% normalize_mono(M, M2)
%
% Transform a monomial in the form:
%   - Coefficient * Var ^ Exponent
%
normalize_mono(M, M2) :-
    monparts(M, K, indep^1),
    M2 = K, 
    !.

normalize_mono(M, M2) :-
    monparts(M, K, VP), 
    M2 = K*VP, 
    !.

%% addmono(M1, M2, M3)
% 
% True if M3 is the sum of M1 with M3.
% If both monomials have the same var power the sum
% results in a polynomial.
%
addmono(M, 0, M) :- !.
addmono(0, M, M) :- !.

addmono(M1, M2, M) :-
    monparts(M1, K1, VP1),
    monparts(M2, K2, VP1),
    K3 is K1+K2,
    M3 = K3 * VP1, 
    simmono(M3, M),
    !.

addmono(M1, M2, M) :-
    monparts(M1, _, VP1),
    monparts(M2, _, VP2),
    VP1 \== VP2,
    M = M1 + M2, 
    !.

%% scalemono(M1, K, M2)
%
% True if M2 is M1 multiplied by the scalar K.
%
scalemono(M1, K, M3) :-
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
simmono(M1, M) :- 
    normalize_mono(M1, M2),
    simmono_aux(M2, M).

simmono_aux(-1*_^0, -1)   :- !.
simmono_aux(1*VP, M2)     :- simmono_aux(VP, M2), !.
simmono_aux(-1*VP, -M2)   :- simmono_aux(VP, M2), !.
simmono_aux(K*indep, K)   :- number(K),           !.
simmono_aux(K*indep^1, K) :- number(K),           !.
simmono_aux(indep, 1)     :- !.
simmono_aux(indep^1, 1)   :- !.
simmono_aux(0*_, 0)       :- !.
simmono_aux(K*V^1, K*V)   :- coefficient(K),      !.
simmono_aux(V^1, V)       :- pvar(V),             !. 
simmono_aux(-V^1, -V)     :- pvar(V),             !. 
simmono_aux(_^0, 1)       :- !.
simmono_aux(K*_^0, K)     :- number(K),           !.
simmono_aux(M, M).

%% mono_compare(Op, M1, M2)
%
% Predicate to use with predsort to sort polymonials and 
% avoid losing similar monomials.
%
% True if Op is < and M1 <= M2 or Op is > and M1 > M2
%
mono_compare(<, M1, M2) :- mono_compare_aux(<, M1, M2), !.
mono_compare(<, M1, M2) :- mono_compare_aux(=, M1, M2), !.
mono_compare(>, M1, M2) :- mono_compare_aux(>, M1, M2), !.

%% mono_compare(++Op, M1, M2)
%
% True if M1 compares to M2 using given operator
%
mono_compare_aux(Op, M1, M2) :-
    monparts(M1, _, Var1^Exp1),
    monparts(M2, _, Var1^Exp2),
    compare(Op, Exp1, Exp2).

mono_compare_aux(Op, M1, M2) :-
    monparts(M1, _, Var1^_),
    monparts(M2, _, Var2^_),
    var_compare(Op, Var1, Var2).

%% same_power(M1, M2)
%
% True if M1 and M2 are monomials with the same var power.
%
same_power(M1, M2) :-
    monparts(M1, _, VP),
    monparts(M2, _, VP).

%% same_power(M1, M2)
%
% True if M1 and M2 are monomials with the same variable.
%
same_var(M1, M2) :-
    monparts(M1, _, Var^_),
    monparts(M2, _, Var^_).

%% nega_mono(M)
%
% True if M1 is a monomila with negative coefficient. 
%
nega_mono(M) :-
    monparts(M, K, _),
    is_negative(K).