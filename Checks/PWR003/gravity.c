// Returns the gravity on planet `planetIdx` in m/s2
double gravity(const int planetIdx) {
  if (planetIdx == 0) // Mercury
    return 3.7;
  if (planetIdx == 1) // Venus
    return 8.9;
  if (planetIdx == 2) // Earth
    return 9.8;
  if (planetIdx == 3) // Mars
    return 3.7;
  if (planetIdx == 4) // Jupiter
    return 23.1;
  if (planetIdx == 5) // Saturn
    return 9.0;
  if (planetIdx == 6) // Uranus
    return 8.7;
  if (planetIdx == 7) // Neptune
    return 11.0;

  return 0.0;
}
