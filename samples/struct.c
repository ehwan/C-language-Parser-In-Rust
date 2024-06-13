struct MyStruct
{
  int a;
  int b;
};
int main()
{
  struct MyStruct s;

  s.a = 1;
  s.b = 2;
  print(s.a);
  print(s.b);
  ++s.b;
  print(s.b);
  return 0;
}