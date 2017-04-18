# Haskell Calculator

A simple four-function visual calculator, implemented in Haskell.

This calculator is capable of evaluating (and visualizing) arithmetic expressions such as "<code>1+1</code>" or "<code>(1+2)*(3+4)</code>".

It supports:
* natural numbers (of arbitrary size);
* addition, subtraction, multiplication, and division;
* sub-expressions nested within parentheses;
* unary negation.

## Demonstration

Here's a [web-based demonstration](https://jonathanknowles.github.io/haskell-calculator-web/) built with [GHCJS](https://github.com/ghcjs/ghcjs) and [Reflex](https://github.com/reflex-frp/reflex-platform). 

## Implementation

The implementation includes:
* Data structures for encoding arithmetic expressions as trees (see <code>[Calculator.Types](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/Types.hs)</code>)
* Functions for parsing arithmetic expressions from textual input (see <code>[Calculator.Parsing](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/Parsing.hs)</code>)
* Functions for pretty-printing arithmetic expressions (see <code>[Calculator.Printing](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/Printing.hs)</code>)
* Functions for evaluating arithmetic expressions (see <code>[Calculator.Evaluation](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/Evaluation.hs)</code>)
* A simple command-line interface (see <code>[Calculator.CLI](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/CLI.hs)</code>)
* A simple graphical user interface (see <code>[Calculator.GUI](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/GUI.hs)</code>)

Also included:
* A simple demonstration of using a [GADT](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type) to restrict the shape of a tree-based data structure (the <code>[Exp](https://github.com/jonathanknowles/haskell-calculator/blob/master/source/library/Calculator/Types.hs)</code> type).

## Building

This project supports both GHC and GHCJS.

### Building the web-based graphical user interface

The web interface is built with GHCJS.

First ensure that you have a recent version of [NodeJS](https://nodejs.org/) installed. (Versions 6.6.0 and 6.7.0 should both work.)

Then run:

<code>./build-web-gui.sh</code>

If the build is successful, it will notify you of where you can find the build output. For example:
```
Open the following file with your web browser to view the graphical user interface:
/path/to/haskell/calculator/.stack-work/install/x86_64-linux/lts-7.15/ghcjs-0.2.1.9007015_ghc-8.0.1/bin/calculator-gui.jsexe/index.html
```

Copy and paste the final line into your web browser to view the user interface.

### Running the command line interface

The command line interface is built with GHC.

To run it, issue the following command:

<code>stack exec calculator-cli</code>

### Running the test suite

The test suite is built with GHC.

To run the suite, issue the following command:

<code>stack test</code>

### Experimenting with the calculator library

To load the calculator library into GHCI, issue the following command:

<code>stack ghci</code>

