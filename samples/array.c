int main()
{
  int i;

  // array with 5 elements
  int arr1[] = { 1, 2, 3, 4, 5 };

  for (i = 0; i < 5; i++)
  {
    print(arr1[i]);
  }
  // 1, 2, 3, 4, 5

  // array without initialization
  int arr2[5];
  for (i = 0; i < 5; i++)
  {
    print(arr2[i]);
  }
  // 0, 0, 0, 0, 0

  // array with partial initialization
  // remaining elements will be initialized to 0 (default value)
  int arr3[5] = { 1, 2, 3 };
  for (i = 0; i < 5; i++)
  {
    print(arr3[i]);
  }
  // 1, 2, 3, 0, 0

  // this will panic
  // int arr4[2] = { 1, 2, 3 };

  int* arr_ptr = arr3;
  arr_ptr[4] = 100;
  arr_ptr[3] = 50;
  for (i = 0; i < 5; i++)
  {
    print(arr3[i]);
  }
  // 1, 2, 3, 50, 100

  return 0;
}