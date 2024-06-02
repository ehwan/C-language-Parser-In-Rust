typedef struct
{
  int age;
} Person;

int factorial(int n);
void swap(int a, int b);

int main()
{
  int a = 10, b = 20;
  Person person;

  int result = factorial(5);
  a = result;

  if (a == 120)
  {
    b = result;
  }

  swap(a, b);

  int numbers[5] = { 1, 2, 3, 4, 5 };
  for (int i = 0; i < 5; i++)
  {
    numbers[i] = factorial(numbers[i]);
  }

  person.age = a + b;

  return 0;
}

int factorial(int n)
{
  if (n <= 1)
    return 1;
  return n * factorial(n - 1);
}

void swap(int a, int b)
{
  int temp = a;
  a = b;
  b = temp;
}