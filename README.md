# haskell-calculator
A simple four-function calculator, implemented in Haskell.

It includes:
* Data structures for encoding arithmetic expressions as trees (see the <code>[Exp](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator.hs)</code> and <code>[UExp](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator.hs)</code> types).
* Functions for parsing arithmetic expressions from textual input (see function <code>[parseUExp](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator.hs)</code>).
* Functions for pretty-printing arithmetic expressions (see function <code>[pretty](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator.hs)</code>).
* A simple command-line interface (see module <code>[Main](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/executable/Main.hs)</code>).

It also includes a simple demonstration of using a [GADT](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type) to restrict the shape of a tree-based data structure (the <code>[Exp](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator.hs)</code> type).

Building
--------
<code>stack build</code>

Running the test suite
----------------------
<code>stack test</code>

Running the calculator CLI
--------------------------
<code>stack exec calculator</code>
