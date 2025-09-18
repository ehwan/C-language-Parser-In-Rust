extern int printf(char const *, ...);
int main() {
  int i;

  for (i = 1; i <= 5; i++) {
    if (i == 3) {
      continue;
    }
    printf("%d\n", i);

    if (i == 4) {
      break;
    }
  }
  // this will print 1, 2, 4

  printf("%d\n", 100);
  return 0;
}