struct MyStruct
{
  int a;
  int b;
};
int main()
{
  int a = (float*)10;
  int b = 20;
  int c = a + b;
  return c;
}