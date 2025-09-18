extern int printf(const char *fmt, ...);
int main() {
  int a = 10;
  printf("%p\n", &a); // this will print stack index
  printf("%d\n", a);

  int *aptr = &a;
  printf("%p\n", aptr);
  printf("%d\n", *aptr);

  *aptr = 20;
  printf("%p\n", aptr);
  printf("%d\n", *aptr);
  printf("%d\n", a);

  printf("%p\n", &aptr);

  return 0;
}