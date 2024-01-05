# Variable scoping in the context of OpenMP

In the context of OpenMP multithreading, and multithreading in general,
**variable scoping** refers to the thread ownership of variables.

Variables can either be `private`, which means they are local to the thread, or
`shared`, which means that all the threads share an instance of the variable.

Deciding which variables are private and which variables are shared is one of
the most important tasks in OpenMP programming. While accesses to private
variables don't require any synchronization, accesses to shared variables,
unless done carefully, can result in data races and incorrect results.
