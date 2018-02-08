# Language Syntax and Grammar

## Basic Syntax

```
def fib(x)
  if x < 3 then
    1
  else
    fib(x - 1) + fib(x - 2);

fib(40);

# Call external function via extern.
extern sin(arg);
```