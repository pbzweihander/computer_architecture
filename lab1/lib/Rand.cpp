#include <cstdlib>

extern "C" {

  void setSeed(unsigned long long seed) {
    srand(seed);
  }
  unsigned long long getRandom() {
    unsigned long long sign = rand() % 2;
    unsigned long long val1 = rand();
    unsigned long long val2 = rand();
    return (unsigned long long) (val1 | (val2 << 32) | sign << 63);
  }
}
