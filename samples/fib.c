int fib(int n)
{
  if (n <= 2)
  {
    return 1;
  }

  return fib(n - 1) + fib(n - 2);
}

int main()
{
  int i = 0;
  for (i = 1; i <= 20; ++i)
  {
    print(fib(i));
  }

  return 0;
}