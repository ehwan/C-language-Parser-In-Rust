extern int printf(char const *, ...);
int main() {
  // const variable
  {
    int a = 10;
    int const b = 20;

    printf("%d, %d\n", a, b);
    // 10, 20

    a = 100;
    // b = 200; // error
    printf("%d, %d\n", a, b);
    // 100, 20
  }

  // array of const variable
  {
    const int a[5] = {1, 2, 3, 4, 5};

    // a[1] = 10; // error

    printf("%d, %d, %d, %d, %d\n", a[0], a[1], a[2], a[3], a[4]);
  }

  // pointer of const variable
  {
    const int a = 10;
    printf("%d\n", a);

    const int *p = &a;
    p = 9; // valid
    // *p = 100; // error
    printf("%d, %d, %d\n", p, *p, a);
  }

  // string
  {
    const char *s = "hello";

    printf("%s\n", s);
    // s[0] = 'H'; // error
  }

  return 0;
}