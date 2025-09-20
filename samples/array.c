extern int printf(char const *, ...);
int main() {
  int i;

  // array with 5 elements
  int arr1[] = {1, 2, 3, 4, 5};

  printf("arr1 size = %d\n", sizeof(arr1));
  for (i = 0; i < 5; i++) {
    printf("arr1[%d] = %d\n", i, arr1[i]);
  }
  // 1, 2, 3, 4, 5

  // array without initialization
  int arr2[5];
  printf("arr2 size = %d\n", sizeof(arr2));
  for (i = 0; i < 5; i++) {
    printf("arr2[%d] = %d\n", i, arr2[i]);
  }
  // 0, 0, 0, 0, 0

  // array with partial initialization
  // remaining elements will be initialized to 0 (default value)
  int arr3[5] = {1, 2, 3};
  for (i = 0; i < 5; i++) {
    printf("arr3[%d] = %d\n", i, arr3[i]);
  }
  // 1, 2, 3, 0, 0

  // this will panic
  // int arr4[2] = { 1, 2, 3 };

  int *arr_ptr = arr3;
  *arr_ptr = 10;
  ++arr_ptr;
  *arr_ptr = 20;
  *(arr_ptr + 1) = 30;
  arr_ptr[2] = 40;
  arr_ptr[3] = 50;
  for (i = 0; i < 5; i++) {
    printf("arr3[%d] = %d\n", i, arr3[i]);
  }
  // 10, 20, 30, 40, 50

  return 0;
}