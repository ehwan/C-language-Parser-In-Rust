int main()
{
  int i;
  int j;
  int k;

  for (i = 1; i <= 5; ++i)
  {
    for (j = 1; j <= 5; ++j)
    {
      for (k = 1; k <= 5; ++k)
      {
        print(i, j, k);
      }
    }
  }
  return 0;
}