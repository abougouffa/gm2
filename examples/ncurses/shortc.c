static int foo = 0;

void check (short int s, unsigned short int c)
{
  if (s != foo)
    exit(1);
  if (c != foo)
    exit(2);
  foo++;
}
