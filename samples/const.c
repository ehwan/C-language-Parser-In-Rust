int main()
{
  // const variable
  {
    int a = 10;
    int const b = 20;

    print(a, b);
    // 10, 20

    a = 100;
    // b = 200; // error
    print(a, b);
    // 100, 20
  }

  // array of const variable
  {
    const int a[5] = { 1, 2, 3, 4, 5 };

    // a[1] = 10; // error

    print(a[0], a[1], a[2], a[3], a[4]);
  }

  // pointer of const variable
  {
    const int a = 10;
    print(a);

    const int* p = &a;
    p = 9; // valid
    // *p = 100; // error
    print(p, *p, a);
  }

  // string
  {
    const char* s = "hello";

    print_str(s);
    // s[0] = 'H'; // error
  }

  return 0;
}