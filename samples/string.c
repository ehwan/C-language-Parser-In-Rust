extern int printf(char const *, ...);
int strlen(char const *s) {
  int len = 0;
  while (*s != 0) {
    ++s;
    ++len;
  }
  return len;
}
int main() {
  char const *s = "Hello, World!";

  printf("result of strlen(%s): %d\n", s, strlen(s));

  return 0;
}