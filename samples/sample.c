int main()
{
  unsigned int ma = (int)10;
  unsigned long long mb = 20;
  int mc = (unsigned long long)ma
           + mb; // WARNING: binary opeartion must be between same types
  print(ma, mb, mc); // SPECIAL built-in function for printing
  ++ma;
  print(ma, mb);
  int i = 0;

  for (i = 0; i < 10; ++i)
  {
    print(i);
  }
  return mc;
}
