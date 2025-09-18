typedef int* int_ptr;
typedef int** int_ptrptr;
int main()
{
  int val[2] = { 10, 20 };

  int_ptr ptr = val;
  int_ptr ptr2 = val + 1;
  print(*ptr, *ptr2);
  // 10, 20

  int_ptrptr ptrptr = &ptr;
  // set val[0] to 100
  **ptrptr = 100;
  ptrptr = &ptr2;
  **ptrptr = 200;

  print(val[0], val[1]);

  return 0;
}