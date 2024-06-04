int factorial(int n);

int main()
{
  int i;
  int a = 10;
  int b = 20;
  int numbers[5];

  int result = factorial(5);
  a = result;

  if (a == 120)
  {
    b = result;
  }
  for (i = 0; i < 5; i++)
  {
    numbers[i] = factorial(numbers[i]);
  }
  return 0;
}

int factorial(int n)
{
  if (n <= 1)
    return 1;
  return n * factorial(n - 1);
}
