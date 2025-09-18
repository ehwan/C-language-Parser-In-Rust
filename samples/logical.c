extern int printf(char const *, ...);
int eval_true() {
  printf("%d\n", 10);
  return 1;
}
int eval_false() {
  printf("%d\n", 20);
  return 0;
}

int main() {
  // only 'eval_false' called
  printf("%d\n", eval_false() && eval_true());
  // both 'eval_true' called
  printf("%d\n", eval_true() || eval_false());

  printf("%d\n", 1 == 1);
  printf("%d\n", 1 != 0);
  printf("%d\n", 0 < 1);
  printf("%d\n", 1 > 0.0);
  printf("%d\n", 0.0 <= 0.1);
  printf("%d\n", 0.1 >= 0.0);

  return 0;
}