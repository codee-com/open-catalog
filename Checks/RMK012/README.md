# RMK012: Conditional execution in the loop body may prevent vectorization

### Issue

The loop appears to be vectorizable, but the statements in the conditions of the
loop render vectorization inefficient.

### Actions

If the conditions in the loop do not depend on the data, remove them using loop
unswitching or loop fission.

### Relevance

The main assumption of [vectorization](../../Glossary/Vectorization.md) is that the
same operation is applied to a vector of data instead of to a single piece of
data. Loops with conditional statements do the opposite: they apply different
operations to the data depending on some condition.

This doesn't mean that vectorization is impossible because conditional
statements can be simulated in a vectorization framework: what was earlier a
conditional statement is now executed unconditionally, but in the final steps
the results are filtered out if the condition is not true. Because the processor
is executing instructions whose result will possibly be thrown away, this
decreases the efficiency of vectorization.

Occasionally, the loops with conditional statements can be vectorized
efficiently:

* If most statements in the loop are executed unconditionally, but there are a
few of them that are executed conditionally, the loop can benefit from
vectorization.

* If the condition in the loop is always evaluated to a  loop-invariant value
(i.e. its value is either true or false across the execution of the loop), this
condition can be moved outside the loop (see
[loop unswitching](../../Glossary/Loop-unswitching.md)).

* If the condition in the loop depends on iterator variables only, the conditions
can be removed by splitting the loop into several loops using
[loop fission](../../Glossary/Loop-fission.md).

### Related resources

* [PWR020: Consider loop fission to enable vectorization](../PWR020/README.md)

* [PWR021: Consider loop fission with scalar to vector promotion to enable vectorization](../PWR021/README.md)

* [PWR022: Move invariant conditional out of the loop to facilitate vectorization](../PWR022/README.md)
