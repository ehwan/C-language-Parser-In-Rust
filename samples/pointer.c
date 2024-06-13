int main()
{
  int a = 10;
  print(&a); // this will print stack index
  print(a);

  int* aptr = &a;
  print(aptr);
  print(*aptr);

  *aptr = 20;
  print(aptr);
  print(*aptr);
  print(a);

  print(&aptr);

  return 0;
}