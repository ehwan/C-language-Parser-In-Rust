int strlen(char* s)
{
  int len = 0;
  while (*s != 0)
  {
    ++s;
    ++len;
  }
  return len;
}
int main()
{
  char* s = "Hello, World!";

  print(strlen(s));

  print_str(s); // built-in function

  return 0;
}