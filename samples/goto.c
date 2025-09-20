extern int printf(char const *, ...);

int main() {
  goto label2;

  printf("Hello, World!\n");

label2:
  printf("Hello, World 2!\n");
  return 0;
}