# Pipes Compiler

Pipes is a simple language for recalculating signals. It is intended
to be possible to compile into instructions for machines where
computation is strictly deterministic, i.e. the program is executed
without any branching, each instruction in order.

## Language

The language allows definition of functions and application of
primitive functions (i.e. instructions of the underlying
machine). Functions support multiple return values (called
definitions). Neither recursive function application nor recursive
definitions are supported.

### Functions

To define a function, the `def` keyword is used:

```
def f(a, b):
  r1 = add(a, b)
  r2 = mul(a, b)
```

Where `add` and `mul` are primitive functions. `r1` and `r2` are
definitions (return values).

### Modules

A file is a module. A module allows definition of multiple functions.


```
def f(a, b):
  r1 = add(a, b)
  r2 = mul(a, b)

def g(x):
  f(a, b)
  r1 = f.r1
  r2 = mul(f.r2, 2)
```

The function `f` from the example above is here used outside of
definitions to make both return values accessible.

## Compiler

The compilation is done in several phases.

### Tokenization

The first phase (PipeLexer) uses a regex tokenizer to produce the
valid tokens of the language.

### Parsing

The second phase (PipeParser) combines the tokens into valid function definitions
based on definitions and basic statements (function applications,
values and constants).

### Plumbing

The third phase (Plumber) uses a entry point function (currently
`main()`) to convert all function definitions into basic
statements. At the end of this phase, the AST contains only primitive
functions (which all have arity 2).
