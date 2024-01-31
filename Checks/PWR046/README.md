# PWR046: Replace two divisions with a division and a multiplication

### Issue

Divisions are expensive operations on modern hardware, so replacing divisions
with cheaper operations often results in speed boost.

### Actions

Replace double division with a division and a multiplication.

### Relevance

Double divisions can be replaced with a division and multiplication, according
to the following patterns:

* `(a / b) / c = a / (b * c)`.

* `a / (b / c) = (a * c) / b`.

### Code example

Have a look at the following code:

```c
float example(float a, float b, float c) {
  return a / b / c;
}
```

The expression `a / b / c` can be rewritten with a single division and a
multiplication, like this:

```c
float example(float a, float b, float c) {
  return a / (b * c);
}
```

>**Note**  
>The compiler does this optimization automatically when
>`-funsafe-math-optimizations` compilation flag is provided.

### Related resources

* [PWR046 examples](../PWR046)

### References

* [Strength reduction](../../Glossary/Strength-reduction.md)

* [IEEE Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_math.html#:~:text=IEEE%20754%20specifies%20exactly%20the,defined%20by%20the%20IEEE%20standard)

* [Semantics of Floating Point Math in GCC](https://gcc.gnu.org/wiki/FloatingPointMath)
