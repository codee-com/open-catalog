# Strength reduction

**Strength reduction** is a program optimization technique that replaces more
expensive operations with cheaper ones.

A few examples of strength reduction:

|                          | Expensive operation | Cheaper operation     |
|--------------------------|---------------------|-----------------------|
| Division by a power of 2 | `a / 2**n`          | `a >> n`              |
| Modulo by a power of 2   | `a % 2**n`          | `a & (2**n - 1)`      |
| Replacement of `pow`     | `pow(a, 0.5)`       | `sqrt(a)`             |
|                          | `pow(a, 1.5)`       | `a * sqrt(a)`         |
|                          | `pow(a, -1.5)`      | `1.0 / (a * sqrt(a))` |
