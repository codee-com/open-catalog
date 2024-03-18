// Returns the gravity on planet `planetIdx` in m/s2
double gravity(const int planetIdx) {
  switch (planetIdx) {
  case 0: // Mercury
    return 3.7;
  case 1: // Venus
    return 8.9;
  case 2: // Earth
    return 9.8;
  case 3: // Mars
    return 3.7;
  case 4: // Jupiter
    return 23.1;
  case 5: // Saturn
    return 9.0;
  case 6: // Uranus
    return 8.7;
  case 7: // Neptune
    return 11.0;
  default:
    return 0.0;
  }
}
