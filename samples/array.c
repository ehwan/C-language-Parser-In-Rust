int main()
{
  // array with 5 elements
  int arr1[] = { 1, 2, 3, 4, 5 };

  // array without initialization
  int arr2[5];

  // array with partial initialization
  // remaining elements will be initialized to 0 (default value)
  int arr3[5] = { 1, 2, 3 };

  // this will panic
  // int arr4[2] = { 1, 2, 3 };

  return 0;
}