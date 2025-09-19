extern int printf(char const *, ...);
int main() {
  int a = 1;
  switch (a) {
  case 0: {
    printf("%d\n", 0);
    break;
  }
  case 1: {
    printf("%d\n", 1);
  }
  case 2: {
    printf("%d\n", 2);
    break;
  }
  default: {
    printf("%d\n", 3);
    break;
  }
  }
  // 1, 2 will be printed

  switch (a) {
  case 2: {
    printf("%d\n", 2);
    break;
  }
  case 1: {
    printf("%d\n", 1);
    break;
  }
  case 0: {
    printf("%d\n", 0);
    break;
  }
  }
  // 1 will be printed

  switch (a) {
  case 0: {
    printf("%d\n", 0);
    break;
  }
  default: {
    printf("%d\n", 1);
  }
  case 2: {
    printf("%d\n", 2);
  }
  case 3: {
    printf("%d\n", 3);
    break;
  }
  }
  // 1, 2, 3 will be printed

  return 0;
}