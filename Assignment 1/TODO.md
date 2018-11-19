# TODO List #

Status of predicate implementation. Serves as documentation.

## Complete ##
 - **pvars(L)**: list of variables to be used

 - **pvar(X)**: True if X is a variable.
  
 - **power(X^Y)**: True if x is a variable and Y is a non-negative integer.
 
 - **varpower(VP, V, E)**: Extract var and exponent from var power. 

 - **coefficient(K)**: True if K Is a number.
  
 - **monomial(M)**: True if M is a monomial.
 
 - **monparts(M, K, V^E)**: Extracts coefficient, var and exponent from a monomial. 

 - **normalize_mono(M, M2)**: Transforms M into the form Coefficient*Var^Exponent

 - **polynomial(P)**: True if P is a sum of monomials.