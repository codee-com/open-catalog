# Row-major and column-major order

In computing, row-major order and column-major order are methods for storing
multidimensional arrays in memory.

![Row-major and column-major memory layout diagram](../static/img/Row-major_column-major.svg#gh-light-mode-only)
![Row-major and column-major memory layout diagram](../static/img/Row-major_column-major-dark.svg#gh-dark-mode-only)

Memory layout, together with
[memory access pattern](Memory-access-pattern.md), is very important
for program's performance.

Best performance is obtained when the layout matches the memory access pattern,
i.e. iterating through a column-major stored matrix column-wise and through a
row-major stored matrix row-wise. This will make for sequential memory access
and the best performance. Techniques like loop interchange can be used to change
the memory access pattern.
