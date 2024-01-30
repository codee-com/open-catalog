# RMK003: potential temporary variable for the loop which might be privatizable, thus enabling the loop parallelization

### Issue

Loop is a potential parallelization opportunity if it can be confirmed that
variable `X` is a temporary.

### Actions

Analyze the data scoping of the variable `X` to determine whether it is a
temporary and privatize it when parallelizing if it is.

### Relevance

Determining the data scoping of all variables in a loop is critical to decide
whether it can be parallelized. Just like scalars, arrays can be loop temporary
variables, meaning that their value is computed and used on each iteration
regardless of other iterations. Since they are only used in a specific
iteration, temporaries can be safely privatized upon loop parallelization. In
order to determine whether an array is a temporary, all its usages must be
completely analyzed. This is not trivial and many tools struggle to do it, even
when they can successfully do so for scalar temporaries. Therefore, the
developer should analyze array usages to detect temporaries and ensure that
their data scoping is private to create the most efficient parallel version of
the loop.
