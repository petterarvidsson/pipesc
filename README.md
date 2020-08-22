# Pipes Compiler

Pipes is a simple language for recalculating signals. It is intended
to be possible to compile into instructions for machines where
computation is strictly deterministic, i.e. the program is executed
without any branching, each instruction in order.

## Language

The language allows definition of functions, midi controllers (a sort
of function), groups (an entity on the SDHI) and application of
functions (including predefined functions). Neither recursive function
application nor recursive definitions are supported.

### Types

Every function argument must define a type. All types are numerical
and allow defining an allowed range. All predefined functions support
converting ranges from arguments to result, so the compiler can derive
the return type (and its range) for all functions. This means that it
is easy to know if the range of a calculation changes into something
that is undefined for the function.

A simple type is defined in the following way:
```
int a

```

A type that supports a specific range is defined like this:
```
int[0..127] a
```

A type can specify non-continuous ranges (for example the divisor of the division function):
```
int[-127..-1,1..127] divisor
```


### Functions

To define a function, the `def` keyword is used:

```
def f(int a, int[0..10] b) =
  add(a, mul(b, b))
```

Where `add` and `mul` are predefined functions. All arguments must be given a type.

### Modules

A file is a module. A module allows definition of multiple functions,
which can call each other.

```
def f(int a, int b) =
  add(a, mul(b, b))

def g(int x) =
   mul(f(1, x), 2)
```

## Software Defined Hardware Interface (SDHI) specific functionality

### Groups

A group defines a grouping of controls to be placed together inside a single frame. For example a low pass filter could define a group for both of its controls (resonance and frequency).

A group is defined like this:
```
group filter(name, from row, from column, to row, to column)
```

The low pass filter example could look something like this:
```
group filter("Filter", 0, 0, 2, 1)
```

### Controllers

A controller is an input device (such as a rotary encoder) that is assigned to a specific group
