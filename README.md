Haskell Calculator
==================

A simple four-function visual calculator, implemented in Haskell.

This calculator is capable of evaluating (and visualizing) arithmetic expressions such as "<code>1+1</code>" or "<code>(1+2)*(3+4)</code>". Expressions can contain natural numbers and any of the standard arithmetic operators: addition, subtraction, multiplication, and division. Unary negation and nesting sub-expressions within parentheses are also supported. (Non-natural numbers are not supported.)

Demonstration
-------------

Here's a [web-based demonstration](https://jonathanknowles.github.io/haskell-calculator-web/) built with [GHCJS](https://github.com/ghcjs/ghcjs) and [Reflex](https://github.com/reflex-frp/reflex-platform). 

Implementation
--------------
The implementation includes:
* Data structures for encoding arithmetic expressions as trees (see <code>[Calculator.Types](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/Types.hs)</code>)
* Functions for parsing arithmetic expressions from textual input (see <code>[Calculator.Parsing](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/Parsing.hs)</code>)
* Functions for pretty-printing arithmetic expressions (see <code>[Calculator.Printing](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/Printing.hs)</code>)
* Functions for evaluating arithmetic expressions (see <code>[Calculator.Evaluation](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/Evaluation.hs)</code>)
* A simple command-line interface (see <code>[Calculator.CommandLineInterface](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/CommandLineInterface.hs)</code>)
* A simple web interface (see <code>[Calculator.WebInterface](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/WebInterface.hs)</code>)

The implementation also includes:
* A simple demonstration of using a [GADT](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type) to restrict the shape of a tree-based data structure (the <code>[Exp](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/Types.hs)</code> type).

Building
--------
For the web interface, first ensure that you have a recent version of [NodeJS](https://nodejs.org/) installed. (Versions 6.6.0 and 6.7.0 should both work.)

Then run:

<code>stack build</code>

Running the test suite
----------------------
<code>stack test</code>

Running the calculator CLI
--------------------------
<code>stack exec calculator</code>
