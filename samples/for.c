int main()
{
  int i;

  for (i = 1; i <= 5; i++)
  {
    if (i == 3)
    {
      continue;
    }
    print(i);

    if (i == 4)
    {
      break;
    }
  }
  // this will print 1, 2, 4

  print(100);
  return 0;
}