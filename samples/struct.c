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

  struct MyStruct* sptr = &s;
  sptr->b = 100;
  print(s.b);

  struct MyStruct s2 = s;
  print(s2.a);
  print(s2.b);
  print(s2.c);
  s2.a = 100;
  s2.b = 200;
  s2.c = 300;

  print(s2.a);
  print(s2.b);
  print(s2.c);
  print(s.a);
  print(s.b);
  print(s.c);

  return 0;
}