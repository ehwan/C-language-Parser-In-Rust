struct MyStruct
{
  int a;
  int b;
};
union MyUnion
{
  int a;
  int b;
};
enum MyEnum
{
  a,
  b = 10,
  c,
  d = 20
};
int take(float**, int* b, struct MyStruct s);
int main()
{
  int a = (int)10;
  int b = 20;
  int c = a + b;
  return c;
}