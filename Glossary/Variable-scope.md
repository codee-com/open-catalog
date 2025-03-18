# Variable scope

In computer programming, the term **variable scope** usually refers to the part
of the code where the variable can be used (e.g. a function, a loop or the whole
program).

In C/C++ there are the following scopes:

* Block scope - a variable defined inside a function has a block scope and is
accessible from the place of definition until the end of the block where it is
defined. In C++, blocks are delimited with curly brackets `{}`.

* Function scope - function arguments have a function scope and can be used
anywhere inside the function.

* File scope - defined outside functions, these variables are visible from the
place of definition until the end of the file.

* Global scope - similar to file scope, but these variables are visible in all
the functions in the program.
