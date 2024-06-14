struct MyStruct
{
  int a;
  int b;
  int c;
};
int main()
{
  struct MyStruct s = { 1, 2, 3 };

  int x = { 10 };

  s.a = 10;
  print(&s.a);
  print(s.a);
  print(s.b);
  print(s.c);

  ++s.c;
  print(s.a);
  print(s.b);
  print(s.c);

  print(x);
  x = 20;
  print(x);
  return 0;
}