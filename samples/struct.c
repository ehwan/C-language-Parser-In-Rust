struct MyStruct
{
  int a;
  int c[2];
  int b[5];
};
int main()
{
  struct MyStruct s;

  s.a = 1;
  // s.b = 2;
  print(&s.a);
  print(s.c);
  print(s.b);

  // ++s.b;
  print(s.b);
  return 0;
}