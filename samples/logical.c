int eval_true()
{
  print(10);
  return 1;
}
int eval_false()
{
  print(20);
  return 0;
}

int main()
{
  // only 'eval_false' called
  print(eval_false() && eval_true());
  // both 'eval_true' called
  print(eval_true() || eval_false());

  print(1 == 1);
  print(1 != 0);
  print(0 < 1);
  print(1 > 0.0);
  print(0.0 <= 0.1);
  print(0.1 >= 0.0);

  return 0;
}