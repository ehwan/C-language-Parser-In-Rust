// fibonacci sequence using recursion declaration
int fibonacci(int);

// main function
int main()
{
  print_str("Hello, World!"); // built in function 'print_str'
  int var = 10;
  int* ptr = &var;
  *ptr = 100;
  print(ptr, *ptr, var); // built in function 'print'

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
