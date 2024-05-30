#ifndef GRAVITY_H
#define GRAVITY_H

#ifdef __GNUC__
  #define CONST_ATTR __attribute__((const))
#else
  #define CONST_ATTR
#endif

double gravity(const int) CONST_ATTR;

#endif // GRAVITY_H
