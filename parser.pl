:-use_module(library(dcg/basics)).
:-consult(poly).

parse(Result) --> command(Result).
parse(Var = Result) --> command(Result), save_op, var(Var),
    { nb_setval(Var, Result) }.

poly_or_var(Poly) --> 
    poly(Poly); 
    var(Var), {value(Var, Poly) }.

var(Var) --> string([VarStr]), 
    { string_to_atom(VarStr, Var) }.

command(Result) --> show_command, poly(Result).
command(Var = Result) --> show_command, var(Var),
    { value(Var, Result) }.
command(Result) --> show_command, ["stored"], (["polynomials"]; []),
    % TODO: filter out environment vars
    { findall(Var=Value, getVar(Var, Value), Result) }.

command(Result) --> sim_command, poly_or_var(Poly),
    { simpoly(Poly, Result) }.

command(Result) --> mult_command, num(Coeff), mult_op, poly_or_var(Poly), 
    { scalepoly(Poly, Coeff, Result) }.
command(Result) --> mult_command, poly_or_var(Poly), mult_op, num(Coeff), 
    { scalepoly(Poly, Coeff, Result) }.

command(Result) --> add_command, poly_or_var(Poly), add_op, poly_or_var(Poly2), 
    { addpoly(Poly, Poly2, Result) }.

command(Result) --> del_command, var(Var), 
    { nb_delete(Var), concat("Deleted ", Var, Result) }.

command(Result) --> help_command, 
    { helpString(Result) }.

getVar(Var, Value) :-
    value(Var, Value), 
    Value \= [].

% Commands --------------------------------------
optional_poly -->  (["polynomial"]; []).

show_command --> 
    ["show"], optional_poly.

add_command  --> 
    (["add"]; ["sum"]), optional_poly.

mult_command --> 
    (["multiply"]; ["scale"]), optional_poly.

sim_command  --> 
    (["simplify"]; ["simple"]), optional_poly.

help_command --> 
    ["help"]; ["commands"].

del_command  --> 
    ["forget"]; ["remove"]; ["delete"].

add_op --> 
    (["to"]; ["with"]; ["and"]), optional_poly.

mult_op --> 
    ["by"], optional_poly.

save_op --> 
    ["as"];
    (["store"], ["to"]).

% Poly/Mono -------------------------------------
poly(Mono) --> mono(Mono).

% no recursion, but gives polynomials in the form Poly+(Poly+(Poly+(...)))
% the implementation at poly.pl can't parse polynomials as a sum of polynomials
% and doing that isn't really the point
%poly(Mono+Poly) --> mono(Mono), poly_op(plus),  poly(Poly).
%poly(Mono-Poly) --> mono(Mono), poly_op(minus), poly(Poly).

% building polynomials like this circumvents chaining, but they
% have to be flipped around to prevent left recursion
% not a pretty hack
poly(Poly+Mono) --> mono(Mono), poly_op(plus), poly(Poly).
poly(PolyMinus+Mono) --> mono(Mono), poly_op(minus), poly(Poly), 
    { scalepoly(Poly, -1, PolyMinus) }.

mono(K) --> coeff(K).
mono(V) --> myvar(V).
mono(K*V) --> coeff(K), myvar(V).
mono(V^E) --> myvar(V), power(E).
mono(K*V^E) --> coeff(K), myvar(V), power(E).

% Syntax ------------------------------------------

coeff(K) --> num(K), (mono_op(times); []).
%coeff(K) --> num(K).

power(E) --> mono_op(power), num(E), { exponent(E) }.
power(2) --> ["squared"].
power(3) --> ["cubed"].

% Operators ----------------------------------------~

poly_op(plus)  --> 
    ["plus"]; 
    ["+"].

poly_op(minus) --> 
    ["minus"]; 
    ["-"].

mono_op(times) --> 
    ["times"]; 
    ["*"].

mono_op(power) --> 
    ["raised"], ["to"]; 
    ["^"].

% Syntax ----------------------------------------

% A character of length 1
myvar(Var) --> string([Str]),  
    { string_length(Str, 1), atom_string(Var, Str) }.

% A number
num(I) --> string([Str]), 
    { number_string(I, Str) }.

% A written number
num(I) --> dig(I).
dig(0) --> ["zero"].
dig(1) --> ["one"].
dig(2) --> ["two"].
dig(3) --> ["three"].
dig(4) --> ["four"].
dig(5) --> ["five"].
dig(6) --> ["six"].
dig(7) --> ["seven"].
dig(8) --> ["eight"].
dig(9) --> ["nine"].

% Helpers ---------------------------------------
helpString('Instructions

All commands are of the form
    command args [as Var]
Where [as Var] is optional and saves the result to Var

Command List

help - Show this prompt. alias: commands

show {Poly} - parse a polynomial, useful if you want to store it for later use.

add {Poly1} [to/with] {Poly2}   - sums two polynomials. alias: sum

multiply {Poly} by {Scalar} - Scale a polynomial. alias: scale

simplify {Poly} - Simplifies a polynomial. alias: simple

forget {Var} - Delete a saved polynomial. alias: delete, remove').

value(Var, Value) :-
    nb_current(Var, Value).