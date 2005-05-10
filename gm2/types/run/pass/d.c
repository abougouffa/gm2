
typedef struct {
  int tag;
  union {
    struct {
      int foo;
      int bar;
      union {
	int bt;
	int bf;
      } inner;
    } first;
    int an;
  } that;
  int final;
} this;

void assert (int v)
{
  if (! v)
    exit(1);
}

void d_test (this *s, int n, int v)
{
  switch (n) {

  case 1: assert(s->tag == v); break;
  case 2: assert(s->that.first.foo == v); break;
  case 3: assert(s->that.first.bar == v); break;
  case 4: assert(s->that.first.inner.bt == v); break;
  case 5: assert(s->that.first.inner.bf == v); break;
  case 6: assert(s->that.an == v); break;
  case 7: assert(s->final == v); break;
  }
}

