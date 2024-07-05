#define MY_MACRO 10
#define MY_ADD(x, y) (y) + (x) *  \
  10

int main()
{
  print_str("MY_MACRO: ");
  print(MY_ADD(MY_MACRO + 1, 20));
  return 0;
}