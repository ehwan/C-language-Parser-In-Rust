extern int printf(char const *, ...);

int fib(int);

int main() {
  int i = 0;
  for (i = 1; i <= 20; ++i) {
    printf("%d\n", fib(i));
  }

  return 0;
}
int fib(int n) {
  if (n <= 2) {
    return 1;
  }

  return fib(n - 1) + fib(n - 2);
}