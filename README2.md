# Logic Programming - Assignment 2

## Grammmars

Demo for a grammar that recognizes polynomials written in natural language.
The demo is also written using a grammar for natural language.

Numbers can be written in written form up to nine. Numeric format is supported for anything else, including floats.

Polynomials can be written as:

- five x raised to seven plus y squared
- two times x cubed minus three v
- 20 w plus 1.5 z

## Running

Run program with `swipl demo.pl`

Start demo with `polyplay.`

## Commands

A prompt with all commands and usage can be brought up with `help` or `commands`.

`command args [as Var]`

[as Var] is an optional argument for storing the result in a variable for later use.

### show

Parses the given polynomial, useful for storing.

`> show Poly`

ex: `show two x squared as P1`

Stored polynomials can be seen with:

`> show stored [Polynomials]`

### forget (alias: remove, delete)

Remove a stored variable.

`> forget Var`

ex: `forget P1`

### add (alias: sum)

Sum two polynomials

`> add/sum Poly1 to/with/and Poly2`

ex: `sum x with y` or `add two times x to three y`

### multiply (alias: scale)

Multiply a polynomial by a scalar

`> multiply Scalar by Polynomial` or `> multiply Polynomial by Scalar`

ex: `scale x cubed by two`

### simplify

Simplify a polynomial

`simplify Polynomial`

ex: `simplify one times x raised to one`