#define MY_MACRO 10
#define MY_ADD(x, y) y + x * 10
#define MY_ADD2(x, y) (y) + (x) * 10

#if MY_MACRO == 10

int main()
{
  print(MY_ADD(MY_MACRO + 1, 20)); // print( 20 + MY_MACRO + 1 * 10 ) = 40
  print(MY_ADD2(MY_MACRO + 1, 20)); // print( (20) + (MY_MACRO + 1) * 10 ) = 130
  return 0;
}

#endif