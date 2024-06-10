int main()
{
  int i = 0;
  while (i < 10)
  {
    if (i == 6)
    {
      break;
    }
    print(i);
    if (i == 3)
    {
      i += 2;
      continue;
    }
    ++i;
  }
  // this will print 0, 1, 2, 3, 5

  return 0;
}