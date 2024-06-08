struct MyStruct
{
  int a;
  int b;
};
int take(float**, int* b, struct MyStruct s);
int main()
{
  int a = (int)10;
  int b = 20;
  int c = a + b;
  return c;
}