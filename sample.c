struct MyStruct
{
  int struct_a;
  int struct_b;
};
union MyUnion
{
  int union_a;
  int union_b;
};
enum MyEnum
{
  enum_a,
  enum_b,
  enum_c = 10,
  enum_d,
  enum_e = 20
};
int take(float**, int* b, struct MyStruct s);
int main()
{
  unsigned int ma = (int)10;
  unsigned long long mb = 20;
  int mc = ma + mb;
  return mc;
}