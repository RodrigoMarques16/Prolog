%% pvars(L)
%
% Used to get the list of variables accepted by the 
% program.
%
pvars([w,x,y,z]).

%% pvar(X)
%
% True if X is a variable accepted by the program.
%
pvar(X) :- pvars(V), member(X,V).
%pvar(X) :- atom(X).

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
power(V^E) :- pvar(V), exponent(E).
power(V)   :- pvar(V).

%% varpower(VP, V, E)
%
% Extracts parts from a var power.
%
varpower(V^E, V, E) :- power(V^E).
varpower(V, V, 1)   :- power(V).

%% var_compare(Op, V1, V2)
%
% True if OP is < and V1 is 'indep' or comes before V2 in 
% lexicographic order.
% True if OP is > and V2 is 'indep' or V1 comes after V2 in 
% lexicographic order.
%
var_compare(<, indep, _) :- !.
var_compare(>, _, indep) :- !.
var_compare(<, X, Y)     :- X @< Y.
var_compare(>, X, Y)     :- X @> Y.