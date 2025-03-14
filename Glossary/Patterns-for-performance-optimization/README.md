# Patterns for performance optimization

### Parallel patterns

Software design patterns are reusable descriptions or templates for how to
**solve recurring problems that commonly appear during programming**. They have
been used for decades to speed up software development while ensuring that
best-practice solutions are implemented. This follows the principle of divide
and conquer where some subproblems are solved through patterns.

**Using patterns facilitates learning and opens the door for advanced tooling**.
Using common categories help students find the solution to a problem quicker.
Tools can also benefit since some solutions may be automatized at least up to a
certain point, such as providing a template for the implementation.

**Parallel code also exhibits recurring patterns**. For instance, it is very
common to perform a mathematical operation over each element of an array.
Another significant example is computing a single scalar value by computing all
the elements in an array. Memory accesses can also be classified into different
patterns. By analyzing the properties of these recurring computation patterns.
Solutions to efficiently implement them in parallel can be developed.

### Computation patterns

* 📄 [Forall](Forall.md)
* 📄 [Scalar reduction](Scalar-reduction.md)
* 📄 [Sparse reduction](Sparse-reduction.md)
* 📄 [Recurrence](Recurrence.md)

### Memory patterns

* 📄 [Linear](Linear.md)
* 📄 [Column-major](Column-major.md)
* 📄 [Row-major](Row-major.md)
