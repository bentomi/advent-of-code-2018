#include <stdlib.h>
#include <stdio.h>

int prog(int r0)
{
  int r2 = 0, r3 = 0, r4 = 0, r5 = 0;

  goto label17;
 label1:
  r4 = 1;
 label2:
  r2 = 1;
 label3:
  r5 = r4 * r2;
  r5 = r5 == r3 ? 1 : 0;
  if (!r5) goto label8;
  r0 += r4;
 label8:
  ++r2;
  r5 = r2 > r3 ? 1 : 0;
  if (!r5) goto label3;
  ++r4;
  r5 = r4 > r3 ? 1 : 0;
  if (!r5) goto label2;
  goto label257;
 label17:
  r3 += 2;
  r3 *= r3;
  r3 *= 19;
  r3 *= 11;
  r5 += 7;
  r5 *= 22;
  r5 += 18;
  r3 += r5;
  switch (r0) {
  case 0:
    goto label1;
  case 1:
    r5 = 27;
  case 2:
    r5 *= 28;
  case 3:
    r5 += 29;
  case 4:
    r5 *= 30;
  case 5:
    r5 *= 14;
  case 6:
    r5 *= 32;
  case 7:
  label33:
    r3 += r5;
  case 8:
    r0 = 0;
  case 9:
    goto label1;
  label257:
  default:
    return r0;
  }
}

int main(int argc, char *argv[])
{
  int r0 = argc > 1 ? atoi(argv[1]) : 0;
  printf("Starting with register 0 = %d.\n", r0);
  printf("Halting with register 0 = %d.\n", prog(r0));
  return 0;
}
