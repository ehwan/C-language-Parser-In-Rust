extern int printf(const char *fmt, ...);

// declaration of function fibonacci sequence
int fibonacci(int);

#define MY_MACRO_FUNC(x, y) y + x

#if MY_MACRO_FUNC(1, 2) == 3

// main function
int main() {
  int var = 10;
  int *ptr = &var;
  printf("Hello, World!\n");
  *ptr = MY_MACRO_FUNC(40, 60);
  printf("%p, %d, %d\n", (void *)ptr, *ptr, var);

  printf("%d\n", MY_MACRO_FUNC(10, 20));

  // print fibonacci sequence
  printf("%s\n", "Fibonacci sequence:");
  int i = 1;
  for (i = 1; i <= 10; i++) {
    printf("for i = %d, ", i);
    printf("%d\n", fibonacci(i));
  }

  return 0;
}

// definition of function fibonacci sequence using recursion
int fibonacci(int n) {
  if (n <= 2)
    return 1;
  else
    return fibonacci(n - 1) + fibonacci(n - 2);
}

#else

THIS WILL BE IGNORED

#endif