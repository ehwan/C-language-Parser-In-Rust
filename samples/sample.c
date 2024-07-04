// fibonacci sequence using recursion declaration
#ifndef MY_SAMPLE_C
  #define MY_SAMPLE_C

  #define MY_MACRO 100
int fibonacci(int);

  #define MY_MACRO_FUNC(x, y) y + x

// main function
int main()
{
  print_str("Hello, World!"); // built in function 'print_str'
  int var = 10;
  int* ptr = &var;
  *ptr = MY_MACRO;
  print(ptr, *ptr, var); // built in function 'print'

  print(MY_MACRO_FUNC(10, 20));

  // print fibonacci sequence
  print_str("Fibonacci sequence:");
  int i;
  for (i = 1; i <= 10; i++)
  {
    print(i, fibonacci(i));
  }

  return 0;
}

// fibonacci sequence using recursion definition
int fibonacci(int n)
{
  if (n <= 2)
    return 1;
  else
    return fibonacci(n - 1) + fibonacci(n - 2);
}

#else

This Will Be Ignored

#endif