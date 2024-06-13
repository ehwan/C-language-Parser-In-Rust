// fibonacci sequence using recursion
int fibonacci(int);

int main()
{
  int var = 10; // Declare an integer variable and initialize it
  int* ptr; // Declare a pointer to an integer

  ptr = &var; // Store the address of var in the pointer ptr

  // Print the value of var
  print(var); // special built in function 'print'

  // Print the address of var using the pointer
  print(ptr); // this will print stack index (address of var)

  // Print the value stored at the address pointed to by ptr
  *ptr = 100;
  print(var); // this will print 100

  // print fibonacci sequence
  int i;
  for (i = 1; i <= 10; i++)
  {
    print(i, fibonacci(i));
  }

  return 0;
}

// fibonacci sequence using recursion
int fibonacci(int n)
{
  if (n <= 2)
    return 1;
  else
    return fibonacci(n - 1) + fibonacci(n - 2);
}
