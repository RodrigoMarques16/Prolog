# TODO List #

Status of predicate implementation. Also serves as documentation.

## Base syntax ##

 - **pvars(L)**: list of variables to be used

 - **pvar(X)**: True if X is a variable.
  
 - **power(X^Y)**: True if x is a variable and Y is a non-negative integer.
 
 - **varpower(VP, V, E)**: Extract var and exponent from var power. 

 - **coefficient(K)**: True if K Is a number.

 - **var_compare(Op, V1, V2)**: Compare variables in lexicographic oredr. Gives 'indep' maximum priority.

## Monomials ##

 - **monomial(M)**: True if M is a monomial.
 
 - **monparts(M, K, V^E)**: Extracts coefficient, var and exponent from a monomial. 

 - **normalize_mono(M, M2)**: Transforms M into the form Coefficient*Var^Exponent

 - **add_mono(M1, M2, P)**: True if M3 is the sum of M1 with M2.

 - **simmono(M1, M2)**: Simplifies monomial M1.

 - **scale_mono(M1, K, M2)**: True if M2 is M1 multiplied by K.

 - **mono_compare(Op, M1, M2)**: Monomial comparator for predsort.
  
## Polynomials ##

 - **polynomial(P)**: True if P is a sum of monomials.

 - **poly2list(P, L)**: Transforms polymonials into lists and vice-versa. 

## Incomplete ##
 
 
 
 - **sort_poly(P1, P2)**: Sort a polymonial's monomials.
    - Need to reduce repeated monomials.

 - **addpoly(P1, P2, P3)**: Sum two monomials.
    - Missing sorting before addition.
    - Missing var power checks.

 - **Negative coefficients** in monomials. Transform subtractions.
  
