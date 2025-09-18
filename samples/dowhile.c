extern int printf(char const *, ...);
int main() {
  int i = 0;
  do {
    printf("%d\n", i);
    if (i == 3) {
      i += 2;
      continue;
    }
    if (i == 8) {
      break;
    }

    ++i;
  } while (i < 10);

  // this will print 0, 1, 2, 3, 5, 6, 7, 8

  return 0;
}