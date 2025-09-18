int main()
{
  int a = 1;
  switch (a)
  {
  case 0:
  {
    print(0);
    break;
  }
  case 1:
  {
    print(1);
  }
  case 2:
  {
    print(2);
    break;
  }
  default:
  {
    print(3);
    break;
  }
  }
  // 1, 2 will be printed

  switch (a)
  {
  case 2:
  {
    print(2);
    break;
  }
  case 1:
  {
    print(1);
    break;
  }
  case 0:
  {
    print(0);
    break;
  }
  }
  // 1 will printed

  switch (a)
  {
  case 0:
  {
    print(0);
    break;
  }
  default:
  {
    print(1);
  }
  case 2:
  {
    print(2);
  }
  case 3:
  {
    print(3);
    break;
  }
  }
  // 1, 2, 3 will be printed

  return 0;
}